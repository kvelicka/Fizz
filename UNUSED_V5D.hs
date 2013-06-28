{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}

module V5D (V5Dfile(..), V5DGrid(..), V5D_field(..), readV5Dfile, showGrid, showFile, dataset) where

import System.IO
import System.IO.Unsafe(unsafePerformIO)
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos

import Control.Monad (liftM)
import qualified Data.ByteString.Lazy as Lazy

import Unsafe.Coerce (unsafeCoerce)

import Data.IORef  (IORef, readIORef, writeIORef, newIORef, modifyIORef)
import Data.Int
import Data.Word
import Data.List (find)
import Data.Maybe
import Data.Char (chr)
import Data.Array.Repa (shapeOfList, DIM3, fromListUnboxed)
import Data.Array.Repa.Index
import qualified Data.Map as M

import qualified CellTypes as Cell
import Dataset
import RectGrid
import Geometries

-- A V5D_field contains information about one timeslice for one of
-- the variables whose samples are recorded in a V5D file.

data V5D_field = V5D_field { nr_rows    :: Int
                       , nr_cols    :: Int
                       , nr_levs    :: Int
                       , minval     :: Maybe Float
                       , maxval     :: Maybe Float
                       , name       :: String
                       , compress   :: Int
                       } deriving Show

-- A V5Dfile records data about a collection of variables 
-- (observables), where the variables may be sampled over a
-- range of time points.  The following data structure holds
-- global information about the collection.  
-- NOTE: the order of items in "grids" is significant, and
-- reflects the order in which grid data is stored on disk;
-- this information is used for example to compute the offset
-- from the start of file to the data for a specific grid at
-- a given time point.

data V5Dfile = V5Dfile { handle     :: Handle
                       , nr_times   :: Int
                       , projection :: Int
                       , tstep_size :: Int
                       , offset     :: Integer
                       , grids      :: [V5D_field]
                       } deriving Show

-- 
-- The V5D file header is a sequence of variable-length fields, 
-- each identified by a specific tag.  The following are
-- symbolic names for file tags.
--

t_ID               :: Int = 0x5635440a  {- hex encoding of "V5D\n" -}

t_VERSION          :: Int = 1000
t_NUMTIMES         :: Int = 1001
t_NUMVARS          :: Int = 1002
t_VARNAME          :: Int = 1003

t_NR               :: Int = 1004
t_NC               :: Int = 1005
t_NL               :: Int = 1006
t_NL_VAR           :: Int = 1007
t_LOWLEV_VAR       :: Int = 1008

t_TIME             :: Int = 1010
t_DATE             :: Int = 1011

t_MINVAL           :: Int = 1012
t_MAXVAL           :: Int = 1013

t_COMPRESS         :: Int = 1014
t_UNITS            :: Int = 1015

t_VERTICAL_SYSTEM  :: Int = 2000
t_VERT_ARGS        :: Int = 2100
t_BOTTOMBOUND      :: Int = 2001
t_LEVINC           :: Int = 2002
t_HEIGHT           :: Int = 2003

t_PROJECTION       :: Int = 3000
t_PROJ_ARGS        :: Int = 3100
t_NORTHBOUND       :: Int = 3001
t_WESTBOUND        :: Int = 3002
t_ROWINC           :: Int = 3003
t_COLINC           :: Int = 3004
t_LAT1             :: Int = 3005
t_LAT2             :: Int = 3006
t_POLE_ROW         :: Int = 3007
t_POLE_COL         :: Int = 3008
t_CENTLON          :: Int = 3009
t_CENTLAT          :: Int = 3010
t_CENTROW          :: Int = 3011
t_CENTCOL          :: Int = 3012
t_ROTATION         :: Int = 3013
t_ROWINCKM         :: Int = 3014
t_COLINCKM         :: Int = 3015

t_END              :: Int = 9999

-- Tags can be of two types: global tags are properties
-- of all grids; local relate to a specific grid.

data TagType = Tglobal | Tlocal deriving Show
data Tag     = Global { gtag::Int, gval::Value }
             | Local  { ltag::Int, lgid::Int, lval::Value }
             deriving Show

tagType :: Int -> TagType
tagType t
  | t `elem` [ t_VARNAME
             , t_NL_VAR
             , t_LOWLEV_VAR
             , t_TIME
             , t_DATE
             , t_MINVAL
             , t_MAXVAL
             , t_UNITS
             ] = Tlocal
  | otherwise = Tglobal

-- Tags are used to mark values; we define the range of value
-- types, then map tags to the type of value that should be
-- associated with the tag in the file.

data ValType = Tint | Tstr Int | Tfloat | Tfarr | Tmap | Targ Int | Tend deriving Show

data Value   = Vint {asInt :: Int} | Vstr {asStr::String} | Vfloat {asFloat::Float}
             | Vfarr Int [Float]
             | Vmap Int Float
             | Varg Int Float
             | Vend {fpos :: Int}
             deriving Show

valType t
  | t `elem` [ t_NUMTIMES, t_NUMVARS
               , t_NR, t_NC, t_NL
               , t_NL_VAR, t_LOWLEV_VAR
               , t_TIME, t_DATE
               , t_COMPRESS, t_PROJECTION
               , t_VERTICAL_SYSTEM ]        = Tint
  | t `elem` [ t_VERSION, t_VARNAME]        = Tstr 10
  | t `elem` [ t_UNITS]                     = Tstr 20
  | t `elem` [ t_MINVAL, t_MAXVAL
               , t_BOTTOMBOUND, t_LEVINC ]  = Tfloat
  | t `elem` [ t_VERT_ARGS, t_PROJ_ARGS]    = Tfarr
  | t `elem` [ t_HEIGHT]                    = Tmap
  | t `elem` [ t_NORTHBOUND, t_WESTBOUND
               , t_ROWINC, t_COLINC
               , t_LAT1, t_LAT2
               , t_POLE_ROW, t_POLE_COL
               , t_CENTLON, t_CENTLAT
               , t_CENTROW, t_CENTCOL
               , t_ROTATION, t_ROWINCKM
               , t_COLINCKM]                = Targ t
  | t == t_END                              = Tend
  | otherwise                               = error "Unknown value type"



-- PARSING 
--
-- Parse headers in the file; we use the Parsec library.  The
-- parser needs to keep track of its offset in the file stream
-- so that we can later calculate the offset to the start of 
-- the first grid data; parser state is used for this.

parseV5D :: GenParser Word8 Int [Tag]
parseV5D = do { parseHeader
              ; ts <- parseTags
              ; return ts
              }
parseHeader :: GenParser Word8 Int ()
parseHeader = do { tag :: Int <- getVal
                 ; len :: Int <- getVal
                 ; setState 8
                 ; if tag /= t_ID && len /= 0
                   then return $ error "Not a V5D file."
                   else return ()
                 }

parseTags :: GenParser Word8 Int [Tag]
parseTags = do { pp <- getState
               ; tag :: Int <- getVal
               ; len :: Int <- getVal
               ; pos <- getState
               ; let pos' = 8+pos+len
               ; setState pos'
               ; if tag == t_END 
                 then return $ [Global t_END (Vend $ pos')]
                 else do { let tt = tagType tag
                         ; let vt = valType tag;
                         ; t <- parseTag tt vt tag
                         ; ts <- parseTags
                         ; return $ t:ts
                         }
               }

parseTag :: TagType -> ValType -> Int -> GenParser Word8 Int Tag
parseTag Tglobal vtype t = do { val <- parseVal vtype
                              ; return $ Global t val
                              }
parseTag Tlocal  vtype t = do { var <- getVal
                              ; val <- parseVal vtype
                              ; return $ Local t var val
                              }

parseVal :: ValType -> GenParser Word8 Int Value
parseVal Tfloat    = getVal >>= (return.Vfloat)
parseVal Tint      = getVal >>= (return.Vint)
parseVal (Tstr n)  = do { b <- count n anyToken
                        ; let p = unsafePerformIO $ 
                                  allocaBytes (n+1) (\tmp->
                                  do { pokeArray tmp (b++[0])
                                     ; p <- peekArray0 (toEnum 0) tmp
                                     ; return $ map (chr.fromEnum) p
                                     })
                        ; return $ Vstr p
                        }

parseVal Tfarr     = do { n :: Int     <- getVal
                        ; a :: [Float] <- mapM (const $ getVal) [1..n]
                        ; return $ Vfarr n a
                        }
parseVal Tmap      = do { i :: Int   <- getVal
                        ; v :: Float <- getVal
                        ; return $ Vmap i v
                        }
parseVal (Targ t)  = getVal >>= (return.(Varg t))
parseVal Tend      = error $ "Processed Tend: Should never happen."


-- Low-level binary IO.
-- 
-- Reading of 32-bit integers, floats, and strings, taking into 
-- account that our machines are big-endian while the data is
-- stored low-endian.
-- UGLY!

getVal :: (Show a, Storable a) => GenParser Word8 Int a
getVal     = do { a <- anyToken
                ; b <- anyToken
                ; c <- anyToken
                ; d <- anyToken
                ; let ai :: Word32 = fromIntegral a
                ; let bi :: Word32 = fromIntegral b
                ; let ci :: Word32 = fromIntegral c
                ; let di :: Word32 = fromIntegral d
                -- ; let v :: Word32 = ai + 256*(bi + 256*(ci + 256*di))
                ; let v :: Word32 = di + 256*(ci + 256*(bi + 256*ai))
                ; return $ unsafeCoerce v
                }
                

-- Convert a stream of word 8's into a stream of values.

stream :: (Show a, Storable a) => Int -> [Word8] -> [a]
stream n vs = case runParser (count n getVal) 0 "" vs of
                Left err -> error $ "Failure in stream: " ++ (show err)
                Right vs -> vs


-- INTERPRETATION
--
-- Convert a list of tags into a V5D file and grid descriptors

{-# NOINLINE cache #-}

cache :: IORef (M.Map String (V5Dfile, (Int,Int,Int)))
cache = unsafePerformIO(return =<< newIORef M.empty)

readV5Dfile :: String -> IO(V5Dfile, (Int, Int, Int))
readV5Dfile fn
    = do { mem <- readIORef cache
         ; if M.member fn mem
           then return $ mem M.! fn
           else do { h <-openBinaryFile fn ReadMode
                   ; b <- Lazy.hGetContents h
                   ; case runParser parseV5D 0 fn (Lazy.unpack b) of
                       Left err -> error $ "Parse error in file: " ++ (show err)
                       Right ds -> do { let v = v5dfile h ds
                                      ; writeIORef cache (M.insert fn v mem)
                                      ; return v
                                      }
                   }
         }

v5dfile :: Handle -> [Tag] -> (V5Dfile, (Int,Int,Int))
v5dfile h fs
    = (V5Dfile h nrt proj time offs gs, (xsz,ysz,zsz))
      where
          nrt  = asInt $ global t_NUMTIMES fs
          com  = asInt $ global t_COMPRESS fs
          proj = asInt $ global t_PROJECTION fs
          nrvs = asInt $ global t_NUMVARS fs
          time = sum $ map gridsize gs
          offs = toInteger.fpos $ global t_END fs
          gs   = map (grid fs com) [0..nrvs-1]
          xsz  = maximum . (map nr_rows) $ gs
          ysz  = maximum . (map nr_cols) $ gs
          zsz  = maximum . (map nr_levs) $ gs

grid :: [Tag] -> Int -> Int -> V5D_field
grid fs comp i
    = V5D_field nr_r nr_c nr_l minv maxv name comp
      where
          nr_r = asInt $ global t_NR fs
          nr_c = asInt $ global t_NC fs
          nr_l = asInt $ ((local t_NL_VAR i fs) `orElse` (global t_NL fs))
          minv = fmap asFloat $ local t_MINVAL i fs
          maxv = fmap asFloat $ local t_MAXVAL i fs
          name = asStr.fromJust $ local t_VARNAME i fs

global :: Int -> [Tag] -> Value
global t = gval . fromJust . find globalTest
           where globalTest (Global t' v) = (t == t')
                 globalTest _             = False


local :: Int -> Int -> [Tag] -> Maybe Value
local t i = liftM lval . find localTest
            where localTest (Local t' i' v) = (t == t' && i == i')
                  localTest _               = False

orElse :: (Maybe a) -> a -> a
(Just v) `orElse` _ = v
Nothing  `orElse` v = v


-- GRID EXTRACTION
-- 
-- Given a V5D file, a grid name, and a time, return a regular cubic
-- grid containing the V5D data extracted from file then decompressed.

-- type V5Ddata = Cells Cell_8 Cell.MyVertex Float
type V5Ddata = [Float]

-- Constant used in V5D to represent missing data; currently
-- only used in "decompress" but defined globally as it may
-- be required in the future, e.g. writing grids.

missing :: Float = 1.0e35

gridsize :: V5D_field -> Int
gridsize g = 8*(nr_levs g) + (nr_rows g)*(nr_cols g)*(nr_levs g)*(compress g)

-- Return a given grid contained within the V5D file for a specified timepoint.
-- At this level, find the grid within the list of grids associated within the
-- file, computing the offet to the start of data for that grid.
dataset :: V5Dfile -> String -> Int -> (V5Ddata, V5D_field)
dataset v5d nm t = case post of
                     []     -> error $ "No grid named "++nm
                     (g:gs) -> (extract g (handle v5d) grid_pos, g)
                   where 
                     (pre,post) = span ((/=nm).name) (grids v5d)
                     grid_pos   = (offset v5d) + (toInteger $ t*tstep_size v5d) + pre_size
                     pre_size   = toInteger.sum $ map gridsize pre

-- Convert the V5D grid to a dataset, given the offset to the start
-- of that grid's data, by unpacking the stream of bytes read from
-- the file starting at that offset.
extract :: V5D_field -> Handle -> Integer -> V5Ddata
extract gr h fpos
    = {- regularCubicData dim1 -}(unpack gr str)
      where
          dim1 = (nr_rows gr - 1, nr_cols gr - 1, nr_levs gr - 1)
          dim = (nr_rows gr, nr_cols gr, nr_levs gr)
          gsz = gridsize gr
          str = unsafePerformIO $ do { hSeek h AbsoluteSeek fpos
                                     ; b <- Lazy.hGetContents h
                                     ; return $ take gsz $ Lazy.unpack b
                                     }

-- Data is stored as a sequence of bytes: its conversion into floats
-- is mediated by a set of decompression factors, one per "level" of
-- data within the grid.  At this point we split the compression 
-- factors from the front of the stream, then re-organise the rest 
-- of the stream into a list of layers to be matched against the 
-- corresponding decompression factors.
unpack :: V5D_field -> [Word8] -> [Float]
unpack gr str
    = concat $ zipWith3 (decompress gr) gas gbs levs
      where
          n         = nr_levs gr
          (gas,gbs) = splitAt n $ stream (2*n) str
          levs      = clumps n (nr_rows gr * nr_cols gr) $ drop (n*8) str

clumps :: Int -> Int -> [a] -> [[a]]
clumps 0   _  _ = []
clumps n csz vs = ts:clumps (n-1) csz ds
                  where
                      (ts,ds) = splitAt csz vs

-- Decompress a single layer of V5D data, given its decompression
-- factor.  Work required depends on the kind of compression used;
-- whether floats have been reduced to 1, 2 or 4 bytes.  The latter
-- case corresponds to no compression.

decompress :: V5D_field -> Float -> Float -> [Word8] -> [Float]
decompress gr a b vs
    = case compress gr of
        1 -> map (bound.(trans 255)) $ vs
        2 -> map (trans 65535) $ short vs
        4 -> stream (nr_rows gr * nr_cols gr) vs
      where
        trans :: Real a => a -> a -> Float
        trans k v | v == k    = missing
                  | otherwise = (float v) * a + b

        short :: [Word8] -> [Word16]
        short []       = []
        short (a:b:vs) = let a' :: Word16 = toEnum . fromEnum $ a
                             b' :: Word16 = toEnum . fromEnum $ b
                         in (256*a' + b') : short vs


        bound :: Float -> Float
        bound = let d  :: Float = b/a
                    l  :: Int   = if a > 0.0000000001 then floor d else 1
                    d' :: Float = d - (float l)
                    aa = a * 0.000001
                in if -254 <= l && l <= 0 && d' < aa 
                   then \v -> if abs(v) < aa then aa else v
                   else id

        float :: Real a => a -> Float
        float = fromRational . toRational


data V5DGrid = V5DGrid { v5d_file  :: String
                       , v5d_field :: String
                       , v5d_time  :: Int
                       }

instance Show V5DGrid where
  show (V5DGrid fnm fld t) = fnm ++ "." ++ fld ++ "." ++ (show t)

instance Dataset V5DGrid Float where
  resource d = v5d_file d
  read_data (V5DGrid fnm fld t)
    = do { (ds, _) <- readV5Dfile fnm
         ; let (vals, grd) = dataset ds fld t
         ; let dimx = (nr_rows grd)
         ; let dimy = (nr_cols grd)
         ; let dimz = (nr_levs grd)
         ; let sh   = shapeOfList [dimx, dimy, dimz] :: DIM3
         ; let n = dimx * dimy * dimz
         ; return $ Grid (fnm++"."++fld++"."++(show t)) 
         				fld sh t 
         				(fromJust.minval $ grd) 
         				(fromJust.maxval $ grd) 
         				(fromListUnboxed (Z :. n :: DIM1) vals)
         }



-- UTILITY

showFile :: V5Dfile -> IO()
showFile v = do { putStrLn $ "Number of time slices: "++(show $ nr_times v)
                ; putStrLn $ "FIELDS: "
                ; mapM_ showGrid (grids v)
                }

showGrid :: V5D_field -> IO()
showGrid gr = do { putStrLn $ "  name: " ++ (show.name $ gr)
                 ; putStrLn $ "   dim: " ++ snr ++ ", " ++ snc ++ ", " ++ snl
                 ; putStrLn $ "   rng: " ++ srng
                 ; putStrLn $ "  ----"
                 }
              where
                 snr = show.nr_rows $ gr
                 snc = show.nr_cols $ gr
                 snl = show.nr_levs $ gr
                 srng = case (minval gr, maxval gr) of
                          (Nothing, Nothing) -> "unknown"
                          (Just lb, Nothing) -> "("++(show lb)++", ?)"
                          (Nothing, Just ub) -> "(?, "++(show ub)++")"
                          (Just lb, Just ub) -> "("++(show lb)++", "++(show ub)++")"

