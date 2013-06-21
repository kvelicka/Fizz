{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeOperators, EmptyDataDecls,
             ScopedTypeVariables, BangPatterns, TypeSynonymInstances,TypeOperators, 
             FunctionalDependencies, ExistentialQuantification #-}

module Main where

import Prelude hiding (lookup)

import Graphics.Rendering.OpenGL.GL(Vertex2(..), Vertex3(..))
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Array.Parallel.Unlifted as U

import Control.Applicative

import Text.ParserCombinators.Poly

import System.IO.Unsafe (unsafePerformIO)
import System.IO
import System.Posix.Files
import GHC.Int
import Numeric

import Data.Bits
import Data.Char
import Data.Array
import Data.List ((!!), elemIndex,zipWith4, nub)
import Data.Maybe
import Data.Word
import Data.Ratio
import qualified Data.ByteString {-.Lazy -} as Fast

import Colour
import Render
import Graphics
import Sample hiding (s)
import FixedPrecision
import CaseTable



-- Dataset -------------------------------


data Z = Z
data a :. b = a :. b

class Dim d where
  size :: d -> Int
  dims :: d -> [Int]
  
instance Dim Z where
  size = const 0
  dims = const []
  
instance Dim a => Dim (a :. Int) where
  size (a :. b) = size a * b
  dims (a :. b) = dims a ++ [b]
  
type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. Int
type DIM3 = DIM2 :. Int

data Grid sh v = Grid { origin  :: String
                      , field   :: String
                      , shape   :: sh
                      , time    :: Int
                      , minv    :: v
                      , maxv    :: v
                      , values  :: U.Array v
                      } deriving Show

type Grid3D a = Grid DIM3 a
type Grid2D a = Grid DIM2 a


dim_x, dim_y, dim_z :: Dim d => d -> Int
dim_x d = let [_,_,x] = dims d in x
dim_y d = let [_,y,_] = dims d in y 
dim_z d = let [z,_,_] = dims d in z

-- AstroData ------------------------------------

data Species = SpD | SpG | SpH | SpHp | SpHe | SpHep | SpHepp | SpHm | SpH2 | SpH2p
             | SpR | SpS | SpH2xD | SpCx | SpCy | SpCz | SpMv
  deriving (Eq,Ord)

instance Show Species where
  show SpD    = "D"
  show SpG    = "G"
  show SpH    = "H"
  show SpHp   = "H+"
  show SpHe   = "He"
  show SpHep  = "He+"
  show SpHepp = "He++"
  show SpHm   = "H-"
  show SpH2   = "H2"
  show SpH2p  = "H2+"
  show SpR    = "R"
  show SpS    = "S"
  show SpH2xD = "H2xD"
  show SpCx   = "Cx"
  show SpCy   = "Cy"
  show SpCz   = "Cz"
  show SpMv   = "Mv"


data Range a = Single  a
             | Range   a a
             | Sampled a a a
               deriving Eq

instance (Show a, Num a) => Show (Range a) where
  show (Single  i)     = show i
  show (Range   i j)   = show i ++"-"++show j
  show (Sampled i j k) = show i ++"-"++show (i+j)++"-"++show k


range_to_list :: (Num a, Enum a) => Range a -> [a]
range_to_list (Single i)      = [i]
range_to_list (Range f t)     = [f..t]
range_to_list (Sampled f s t) = [f,s+f .. t]

range_size :: Integral a => Range a -> a
range_size (Single _)      = 1
range_size (Range f t)     = t - f + 1
range_size (Sampled f s t) = ((t - f) `div` s) + 1

type Time = Int

data VisData = From (Range Int) (Range Int) (Range Int) Time Species
               deriving Eq

instance Show VisData where
  show (From x y z t s) = concat [ "x", (show x)
                                 , "y", (show y)
                                 , "z", (show z)
                                 , "t", (show t)
                                 , ".", (show s)
                                 ]

astroFull :: Time -> Species -> VisData
astroFull t s = From (Range 0 599) (Range 0 247) (Range 0 247) t s

astroFour :: Time -> Species -> VisData
astroFour t s = From (Sampled 0 4 599) (Sampled 0 4 247) (Sampled 0 4 247) t s

-- Parsers

species :: Parser Char Species
species = do satisfy (=='D'); return SpD
          `onFail`
          do satisfy (=='G'); return SpG
          `onFail`
          do satisfy (=='S'); return SpS
          `onFail`
          do satisfy (=='R'); return SpR
          `onFail`
          do satisfy (=='C');
             (do satisfy (=='x'); return SpCx
              `onFail`
              do satisfy (=='y'); return SpCy
              `onFail`
              do satisfy (=='z'); return SpCz)
          `onFail`
          do satisfy (=='M'); satisfy (=='v'); return SpMv
          `onFail`
          do satisfy (=='H');
             (do satisfy (=='-'); return SpHm
              `onFail`
              do satisfy (=='+'); return SpHp
              `onFail`
              do satisfy (=='2');
                 (do satisfy (=='+'); return SpH2p
                  `onFail`
                  do satisfy (=='x');
                     satisfy (=='D'); return SpH2xD
                  `onFail`
                  return SpH2)
              `onFail`
              do satisfy (=='e');
                 (do satisfy (=='+')
                     (do satisfy (=='+'); return SpHepp
                      `onFail`
                      return SpHep)
                  `onFail`
                  return SpHe)
              `onFail`
              return SpH)


slice :: Parser Char VisData
slice = do satisfy (=='x'); x <- (parse_range integer)
           satisfy (=='y'); y <- (parse_range integer)
           satisfy (=='z'); z <- (parse_range integer)
           satisfy (=='t'); t <- integer
           satisfy (=='.'); ss <- species
           return $ From x y z t ss

parse_range :: Num a => Parser Char a -> Parser Char (Range a)
parse_range p = do i <- p
                   (do satisfy (=='-')
                       j <- p
                       (do satisfy (=='-')
                           k <- p
                           return (Sampled i (j-i) k)
                        `onFail`
                        return (Range i j))
                    `onFail`
                    return (Single i))

integer :: Parser Char Int	-- positive only
integer = do cs <- many1 (satisfy isDigit)
             return (foldl1 (\n d-> n*10+d)
                            (map digitToInt cs))

-- Low-level IO and conversion 

read_astro_file :: (U.IOElt v, U.Elt v, Ord v, Fractional v) => String -> IO (Grid3D v)
read_astro_file str
    = read_astro (result . fst . runParser slice $ str)
      where result (Left err) = error err       
            result (Right ok) = ok
            
read_astro_double :: VisData -> IO (Grid3D Double)
read_astro_double d@(From xr yr zr t sp)
    = do let basename = show d
         let summary = basename ++ ".summary"
         let dim = Z :. range_size zr :. range_size yr :. range_size xr
         h <- openFile ("repa_"++basename++".dat") ReadMode    
         arr :: U.Array Double <- U.hGet h
         haveSummary <- fileExist summary    
         [min, max] <- if haveSummary
                          then do v <- Fast.readFile summary
                                  return [bytesToValues 0 v, bytesToValues 1 v]
                          else return $ compute_bounds arr
         return $ Grid basename (show sp) dim t min max arr

read_astro :: (U.IOElt v, U.Elt v, Ord v, Fractional v) => VisData -> IO (Grid3D v)
read_astro d@(From xr yr zr t sp)
    = do let basename = show d
         let summary = basename ++ ".summary"
         let dim = Z :. range_size zr :. range_size yr :. range_size xr
         h <- openFile ("repa_"++basename++".dat") ReadMode    
         arr <- U.hGet h
         haveSummary <- fileExist summary    
         [min, max] <- if haveSummary
                          then do v <- Fast.readFile summary
                                  return [bytesToValues 0 v, bytesToValues 1 v]
                          else return $ compute_bounds arr
         return $ Grid basename (show sp) dim t min max arr

compute_bounds :: (U.Elt n, Ord n) => U.Array n -> [n]
compute_bounds arr = [U.fold1 min arr, U.fold1 max arr]
  
bytesToValues :: Fractional a => Int -> Fast.ByteString -> a
bytesToValues !i bs
    = let start = fromIntegral $ i*4
          a = bs `Fast.index` start
          b = bs `Fast.index` (start + 1)
          c = bs `Fast.index` (start + 2)
          d = bs `Fast.index` (start + 3)
          a16 :: Int16 = fromIntegral a `shiftL` 8
          b16 :: Int16 = fromIntegral b 
          c16 :: Int16 = fromIntegral c `shiftL`  8
          d16 :: Int16 = fromIntegral d    
          exp = a16 .|. b16
          man = c16 .|. d16
          val | exp < 0   = toInteger man % (10 ^ negate (toInteger exp))
              | otherwise = toInteger man * (10^exp) % 1  
      in realToFrac val

-- RectGrid -------------------------------------------------------------------

-- Implementation of a dataset with a regular, rectangular
-- grid as topology.  This is essentially the dataset
-- type used in the Vis'06 paper.

data Cell_8 a = Cell_8 !a !a !a !a !a !a !a !a deriving (Eq,Ord,Show)

instance  (Ix a) => Ix (Cell_8 a)  where
    range ((Cell_8 l1 l2 l3 l4 l5 l6 l7 l8),(Cell_8 u1 u2 u3 u4 u5 u6 u7 u8)) =
          [(Cell_8 i1 i2 i3 i4 i5 i6 i7 i8) 
                                     | i8 <- range (l8,u8)
                                     , i7 <- range (l7,u7)
                                     , i6 <- range (l6,u6)
                                     , i5 <- range (l5,u5)
                                     , i4 <- range (l4,u4)
                                     , i3 <- range (l3,u3)
                                     , i2 <- range (l2,u2)
                                     , i1 <- range (l1,u1) ]
	-- Note little-endian order of the 8-tuple.

    index ((Cell_8 l1 l2 l3 l4 l5 l6 l7 l8),(Cell_8 u1 u2 u3 u4 u5 u6 u7 u8))
          (Cell_8 i1 i2 i3 i4 i5 i6 i7 i8) =
       index (l1,u1) i1 + rangeSize (l1,u1) * (
        index (l2,u2) i2 + rangeSize (l2,u2) * (
         index (l3,u3) i3 + rangeSize (l3,u3) * (
          index (l4,u4) i4 + rangeSize (l4,u4) * (
           index (l5,u5) i5 + rangeSize (l5,u5) * (
            index (l6,u6) i6 + rangeSize (l6,u6) * (
             index (l7,u7) i7 + rangeSize (l7,u7) * (
              index (l8,u8) i8)))))))

    inRange ((Cell_8 l1 l2 l3 l4 l5 l6 l7 l8),(Cell_8 u1 u2 u3 u4 u5 u6 u7 u8))
            (Cell_8 i1 i2 i3 i4 i5 i6 i7 i8) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
           && inRange (l5,u5) i5
           && inRange (l6,u6) i6
           && inRange (l7,u7) i7
           && inRange (l8,u8) i8

instance Functor Cell_8 where
  fmap f (Cell_8 v1 v2 v3 v4 v5 v6 v7 v8) 
    = Cell_8 (f v1) (f v2) (f v3) (f v4) (f v5) (f v6) (f v7) (f v8)

instance Applicative Cell_8 where
  pure f = Cell_8 f f f f f f f f
  (Cell_8 f1 f2 f3 f4 f5 f6 f7 f8) <*> (Cell_8 v1 v2 v3 v4 v5 v6 v7 v8)
    = Cell_8 (f1 v1) (f2 v2) (f3 v3) (f4 v4) (f5 v5) (f6 v6) (f7 v7) (f8 v8)

instance (Bounded a) => Bounded (Cell_8 a) where
  minBound = let x = minBound in Cell_8 x x x x x x x x
  maxBound = let x = maxBound in Cell_8 x x x x x x x x

instance (Enum a, Bounded a) => Enum (Cell_8 a) where
  toEnum x = let step = 1 + fromEnum (maxBound`asTypeOf`contents)
                 (sa,a) = x  `divMod` step
                 (sb,b) = sa `divMod` step
                 (sc,c) = sb `divMod` step
                 (sd,d) = sc `divMod` step
                 (se,e) = sd `divMod` step
                 (sf,f) = se `divMod` step
                 (sg,g) = sf `divMod` step
                 (sh,h) = sg `divMod` step
                 contents = toEnum a
             in Cell_8 contents   (toEnum b) (toEnum c) (toEnum d)
                       (toEnum e) (toEnum f) (toEnum g) (toEnum h)
  fromEnum (Cell_8 a b c d e f g h) = 
      let step = 1 + fromEnum (maxBound`asTypeOf`a) in
      fromEnum a + step*
       (fromEnum b + step*
        (fromEnum c + step*
         (fromEnum d + step*
          (fromEnum e + step*
           (fromEnum f + step*
            (fromEnum g + step*
              (fromEnum h)))))))

-- Make Cell_8 an instance of Cell; note the order of nodes in the vertices
-- mapping is important, and corresponds to the enumeration of the cell 
-- vertices used by the marching cube table.

zipWith8 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) =
    z a b c d e f g h : zipWith8 z as bs cs ds es fs gs hs
zipWith8 _ _ _ _ _ _ _ _ _ = []
                                    
instance Cell Cell_8 MyVertex where
  --data Facet Cell_8 x = FacTri x x x
  select n (Cell_8 a b c d e f g h)
    = case n of
        A  -> a
        B  -> b
        C  -> c
        D  -> d
        E  -> e
        F  -> f
        G  -> g
        H  -> h
  mc_case = let table = array (minBound,maxBound::Cell_8 Bool)
                              (map (\ (a,b)-> ( markingToCellBool a
                                              , concatMap (map edgeToPair) b) )
                                   (cell_table_verts cube))
                markingToCellBool m = let q v = v`elem`m in
                    Cell_8 (q A) (q B) (q C) (q D) (q E) (q F) (q G) (q H)
                edgeToPair (Edge a b) = (a,b)
            in (table!)

-- Generate a dataset consisting of the coordinates in a (xsz x ysz x zsz)-cube.
-- Note that the origin of the cube is (0,0,0), and that the components refer
-- to the number of CELLS along each dimension.
cubicGeom :: (Floating a) => Grid DIM3 v -> Stream Cell_8 MyVertex (Vertex3 a)
cubicGeom f 
    = cubicGrid (shape f) $ (cubicPoints f)

-- Generate a list of coordinates for a (xsz x ysz x zsz)-cube, starting
-- from (0,0,0).
cubicPoints :: (Num a) => Grid DIM3 v -> [Vertex3 a]
cubicPoints g = [ Vertex3 (fromIntegral i) (fromIntegral j) (fromIntegral k)
                | k <- [0 .. dim_z (shape g) - 1]
                , j <- [0 .. dim_y (shape g) - 1]
                , i <- [0 .. dim_x (shape g) - 1]
                ]

-- Generate a stream (dataset) of 8-tuple cell samples taken from
-- an input stream of values.  The dataset is an (xmax x ymax x zmax)
-- cube where the components here refer to the size of a dimension 
-- in POINTs.
cubicGrid :: DIM3 -> [a] -> Stream Cell_8 MyVertex a
cubicGrid (Z :. zmax :. ymax :. xmax) 
    = Stream . (discontinuities (0,0,0)) . zipCube
      where
          zipCube stream = zipWith8 Cell_8 stream
                                           (drop 1 stream)
                                           (drop (line+1) stream)
                                           (drop line stream)
                                           (drop plane stream)
                                           (drop (plane+1) stream)
                                           (drop (plane+line+1) stream)
                                           (drop (plane+line) stream)
          line  = xmax
          plane = (xmax)*(ymax)

          discontinuities _ [] = []
          discontinuities (i,j,k) (x:xs)
              | k==(zmax-1)   = []
              | j==(ymax-1)   =    discontinuities (0,0,k+1) (drop (xmax-1) xs)
              | i==(xmax-1)   =    discontinuities (0,j+1,k) xs
              | otherwise     = x: discontinuities (i+1,j,k) xs

-- Cell Types ---------------------------------------------------------
          
class (Functor c, Enum v) => Cell c v | c -> v where
  --data Facet c :: * -> *
  select   :: v  -> c a -> a
  mc_case  :: c Bool -> [(v,v)] --[Facet c (v,v)]

newtype (Enum v, Cell c v) => Stream c v a = Stream { stream :: [c a] }
  
newtype (Enum v, Cell c v) => Cells c v a = Cells { cells :: [c a] }
                              deriving (Show)

-- Pairing allows us to take two datasets (which should have the same
-- dimensionality, but this is not checked), and construct a dataset
-- containing pairs of values.
pair :: (Applicative c, Enum v, Cell c v) => Cells c v a -> Cells c v b -> Cells c v (a,b)
pair (Cells as) (Cells bs) 
  = Cells $ zipWith (\a b -> pure (,) <*> a <*> b) as bs

splitDS :: (Functor c, Enum v, Cell c v) => Cells c v (a,b) -> (Cells c v a, Cells c v b)
splitDS (Cells cs) 
  = (Cells $ map (fmap fst) cs, Cells $ map (fmap snd) cs)

instance (Functor c, Enum v, Cell c v) => Functor (Cells c v) where
  fmap f (Cells d) = Cells (map (fmap f) d)

instance (Applicative c, Enum v, Cell c v) => Applicative (Cells c v) where
  pure f = Cells (repeat (pure f))
  (Cells a) <*> (Cells b) = Cells (zipWith (<*>) a b)


---- test data
data MyVertex = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P
  deriving (Eq,Show,Enum)

icube :: PolyTope Int
icube    = mkPolyTope [ [0,1,2,3], [4,5,6,7], [0,1,4,5]
                      , [2,3,6,7], [0,3,4,7], [1,2,5,6] ]


cube :: PolyTope MyVertex -- 3D
cube     = mkPolyTope [ [A,B,C,D], [E,F,G,H], [A,B,F,E]
                      , [C,D,H,G], [A,D,H,E], [B,C,G,F] ]

-- first arg is scalar
locateCube :: GL.GLfloat -> MyVertex -> GL.Vertex3 GL.GLfloat
locateCube n A = Vertex3  0.0 n   0.0
locateCube n B = Vertex3  n   n   0.0
locateCube n C = Vertex3  n   0.0 0.0
locateCube n D = Vertex3  0.0 0.0 0.0
locateCube n E = Vertex3  0.0 n   n
locateCube n F = Vertex3  n   n   n
locateCube n G = Vertex3  n   0.0 n
locateCube n H = Vertex3  0.0 0.0 n


-- Maths -------------------------------------------------

-- Linear interpolation, and inverse linear interpolation, is a fundamental
-- operation, which is used for (scalar) values, coordinates, colours, and
-- other data.  We define classes to capture these two operations, and here
-- define basic instances (on floating point values), and a more general
-- version of interpolation for any applicative structure.

toFloat :: (Real a, Fractional b) => a -> b
toFloat = realToFrac

class Interp b where
  interp :: Float -> b -> b -> b

class Real b => InvInterp b where
  inv_interp :: b -> b -> b -> Float

instance Interp Float where
  interp t v1 v2 = (1-t)*v1 + t*v2

instance Interp GL.GLfloat where
  interp t v1 v2 = let tf = realToFrac t in (1-tf)*v1 + tf*v2

instance Interp Double where
  interp t v1 v2 = let tf = realToFrac t in (1-tf)*v1 + tf*v2

instance Interp Word8 where
  interp t v1 v2 = toEnum.fromEnum.round $ (1-t)*(toFloat v1) + t*(toFloat v2)

instance InvInterp Float where
  inv_interp s v1 v2 = (s - v1) / (v2 - v1)

instance InvInterp Double where
  inv_interp s v1 v2 = realToFrac $ (s - v1) / (v2 - v1)

instance InvInterp Word8 where
  inv_interp s v1 v2 = toFloat (s - v1) / (toFloat (v2 - v1))

instance (Interp a, Applicative f) => Interp (f a) where
  interp t x y = pure (interp t) <*> x <*> y

-- Algorithms -----------------------------------------

-- Generate the isosurface at a given threshold. Note that the surface is 
-- returned implicitly as a list of points - adjacent groups of three points
-- define the triangles of the surface.
-- (A list of points rather than triangles is used, as it matches better the
-- interface to OpenGL).

isosurface :: (Interp a, InvInterp a, Interp g, Cell c v, Enum v) =>
    a -> Stream c v a -> Stream c v g -> [[g]]
isosurface th samples geom
    = zipWith (surf_cell th) (stream samples) (stream geom)

surf_cell :: (Interp a, InvInterp a, Interp g, Cell c v, Enum v) =>
    a -> c a -> c g -> [g]
surf_cell th sample geom
    = map (surf_geom th sample geom) $ mc_case $ fmap (>th) sample

surf_geom :: (Interp a, InvInterp a, Interp g, Cell c v, Enum v) =>
    a -> c a -> c g -> (v,v) -> g
surf_geom th sample geom (v0,v1)
    = interp (inv_interp th samp_0 samp_1) geom_0 geom_1
      where
          samp_0 = select v0 sample
          samp_1 = select v1 sample
          geom_0 = select v0 geom
          geom_1 = select v1 geom



-- Picture DSL -----------------------------------------



-- 1. Colours -------------------------------------------------------
--
-- We are typically interested in palettes of multiple colours,
-- and provide various methods for constructing these.  For
-- cases where we only want one colour, we also provide an
-- easy way of generating the appropriate (singleton) palette.
--
-- A colour scheme is either a brewer palette, the 
-- procedural colour generator from Vis5D, or a single
-- X11 colour specified by a (standard) name.

data Colour = Brewer Palette | Vis5D | X11 [ColourName] deriving Show

-- For ease of specification, we define some simple "smart
-- constructors" for building colour schemes, applying the
-- suitable wrapper.  Note that the constructor function is 
-- simply the lower case version of the data constructor.

reds, blues :: Colour
reds    = Brewer Reds
blues   = Brewer Blues

blue, red, green, orange, white, yellow :: Colour
blue    = X11 [Blue]
red     = X11 [Red]
green   = X11 [Green]
orange  = X11 [Orange]
white   = X11 [White]
yellow  = X11 [Yellow]


-- 2. Data Expressions 

data DataExpr v = Use VisData deriving Show

fromFull :: Time -> Species -> VisData
fromFull t s = astroFull t s

from4 :: Time -> Species -> VisData
from4 t s = astroFour t s

data Real v => Picture v = Contour Colour (Range v) (Grid2D v) -- (DataExpr v)
                         | ASurface Colour (Range v) (Grid3D v) -- (DataExpr v)
                         | Surface Colour (Range v) (Grid3D v) -- (DataExpr v)
                         | Volume Colour (DataExpr v)
                         | Slice  Colour (DataExpr v)
                         | Hedgehog Colour (DataExpr v) (DataExpr v) (DataExpr v)
                         | Scatter (DataExpr v) (DataExpr v) (DataExpr v)
                         | Draw [Picture v]
                         | Anim [Picture v]

transfer :: Real v => Colour -> GL.GLfloat -> v -> v -> v -> GL.Color4 GL.GLfloat
transfer Vis5D _     minv maxv 
    = transfer_f (minv, maxv)
transfer (Brewer color) alpha minv maxv 
    = transfer_t (minv, maxv) (brewer color alpha)
transfer (X11 names) alpha minv maxv
    = transfer_t (minv, maxv) (map (x11_rgb alpha) names)


{-# SPECIALISE eval_picture :: Picture Double -> HsScene  #-}
eval_picture :: (Enum a, Interp a, InvInterp a, U.Elt a) => Picture a -> HsScene
               
{-
eval_picture (Surface pal levels field)
    = Group static geomlist
      where
          mkgrid = cubicGrid (shape field)         -- ::   [a] -> Cells Cell_8 MyVertex a
          points = mkgrid $ cubicPoints field      -- ::   Stream Cell_8 Vertex3
          vcells = mkgrid $ U.toList $ values field  -- ::   Stream Cell_8 Double
          t_vals = range_to_list levels
          colour = transfer pal 1.0 1.0 (toFloat.length $ t_vals)
          contours = map (\t -> concat $ isosurface t vcells points) $ t_vals
          --arr = values field
          --surf t = concat $ isosurf t arr
          --contours = map surf t_vals
          -- contours = map (\t -> Algorithms.isosurface t (Stream . toList .  values $ field) {- (values field) -}) $ t_vals
          geomlist = --unsafePerformIO(
                     --     (putStrLn $ "Input vcells :") >> 
                     --     (mapM_ (putStrLn.show) (stream vcells)) >> 
                     --     (putStrLn $ "Input points :") >>
                     --     (mapM_ (putStrLn.show) (stream points))
                     --     ) `seq` 
                     zipWith surface_geom contours $ repeat (map colour [1.0 .. (toFloat.length $ t_vals)])
          showit :: [a] -> IO()
          showit x = putStrLn (show $ length x)
-}

eval_picture (Surface pal levels field)
    = Group static geomlist
      where
          mkgrid = cubicGrid (shape field)         -- ::   [a] -> Cells Cell_8 MyVertex a
          t_vals = range_to_list levels
          colour = transfer pal 1.0 1.0 (toFloat.length $ t_vals)
          geomlist = zipWith surface_geom 
                       (map (\t -> concat $ isosurface t (mkgrid $ U.toList $ values field) 
                                                         (mkgrid $ cubicPoints field)) 
                            t_vals)
                       (repeat (map colour [1.0 .. (toFloat.length $ t_vals)]))

{-
Heap Usage -- up to 50 MB

eval_picture (Surface pal levels field)
    = Group static geomlist
      where
          mkgrid = cubicGrid (shape field)         -- ::   [a] -> Cells Cell_8 MyVertex a
          t_vals = range_to_list levels
          points = mkgrid $ cubicPoints field      -- ::   Stream Cell_8 Vertex3
          colour = transfer pal 1.0 1.0 (toFloat.length $ t_vals)
          geomlist = zipWith surface_geom 
                       (map (\t -> concat $ isosurface t (mkgrid $ U.toList $ values field) 
                                                         points) 
                            t_vals)
                       (repeat (map colour [1.0 .. (toFloat.length $ t_vals)]))
-}
eval_picture (Draw ps) = Group static $ map eval_picture ps


exec :: (Enum v, Interp v, InvInterp v, U.Elt v) => Picture v -> IO()
exec p = do { GLUT.initialize "Astro Viewer" [] >> return ()
            ; g <- openGraphics "Scene Viewer" (1000,800)
            ; addScene g (Vertex3 0 0 0, Vertex3 150 62 62) $ [eval_picture p]
            ; GLUT.mainLoop
            }

-- main: if compiling, you must come up with a Picture expression here
main :: IO ()
main = do { ds :: Grid3D Double <- read_astro_double (from4 60 SpG) --(from4 60 G) 
          ; let dminv = minv ds
          ; let dmaxv = maxv ds  
          ; let pic = Surface red  (Single 16000) ds
          ; exec pic
          }
