{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}

{- Interface to datasets from the IEEE Visualization contest 2008.
   This file implements a set of interfaces for reading, accessing
   and manipulating these data.
-}

module AstroData where

import Text.ParserCombinators.Poly
import Data.Char
import Dataset

--import RectGrid
import Dataset
import Data.Array.Parallel.Unlifted (Elt)
import Data.Array.Repa (Array, fromList, toIndex, Shape, DIM3, DIM2, Z(..), (:.)(..), (!:), foldAll, force, fromIndex, extent)
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

import System.IO
import System.Posix.Files
import Data.Bits
import FixedPrecision
import Sample hiding (s)
import Data.Word
import qualified Data.ByteString.Lazy as Fast
import GHC.Int
import Data.Ratio
import Numeric

type Grid3D a = Grid DIM3 a
type Grid2D a = Grid DIM2 a


-- Dataset definitions ------------------------------------------------------
data Species = D | G | H | Hp | He | Hep | Hepp | Hm | H2 | H2p
             | R | S | H2xD | Cx | Cy | Cz | Mv
  deriving (Eq,Ord)

instance Show Species where
  show D    = "D"
  show G    = "G"
  show H    = "H"
  show Hp   = "H+"
  show He   = "He"
  show Hep  = "He+"
  show Hepp = "He++"
  show Hm   = "H-"
  show H2   = "H2"
  show H2p  = "H2+"
  show R    = "R"
  show S    = "S"
  show H2xD = "H2xD"
  show Cx   = "Cx"
  show Cy   = "Cy"
  show Cz   = "Cz"
  show Mv   = "Mv"


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

{-
data Slice = Slice { x :: Range Int
                   , y :: Range Int
                   , z :: Range Int
                   , t :: Range Int
                   , s :: [Species]
                   }
  deriving (Eq,Show)
-}
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

type AstroData = Grid Float

astroFull :: Time -> Species -> VisData
astroFull t s = From (Range 0 599) (Range 0 247) (Range 0 247) t s

astroFour :: Time -> Species -> VisData
astroFour t s = From (Sampled 0 4 599) (Sampled 0 4 247) (Sampled 0 4 247) t s

sliceZ :: Int -> VisData -> VisData
sliceZ z (From x y _ t s) = (From x y (Single z) t s)

{-
showSlice :: Slice -> String
showSlice slice = "x"++show (x slice)
                  ++"y"++show (y slice)
                  ++"z"++show (z slice)
                  ++"t"++show (t slice)
                  ++"."++concatMap show (s slice)

readSlice :: String -> Either String Slice
readSlice = fst . runParser slice
-}
-- Parsers ------------------------------------------------------------------
species :: Parser Char Species
species = do satisfy (=='D'); return D
          `onFail`
          do satisfy (=='G'); return G
          `onFail`
          do satisfy (=='S'); return S
          `onFail`
          do satisfy (=='R'); return R
          `onFail`
          do satisfy (=='C');
             (do satisfy (=='x'); return Cx
              `onFail`
              do satisfy (=='y'); return Cy
              `onFail`
              do satisfy (=='z'); return Cz)
          `onFail`
          do satisfy (=='M'); satisfy (=='v'); return Mv
          `onFail`
          do satisfy (=='H');
             (do satisfy (=='-'); return Hm
              `onFail`
              do satisfy (=='+'); return Hp
              `onFail`
              do satisfy (=='2');
                 (do satisfy (=='+'); return H2p
                  `onFail`
                  do satisfy (=='x');
                     satisfy (=='D'); return H2xD
                  `onFail`
                  return H2)
              `onFail`
              do satisfy (=='e');
                 (do satisfy (=='+')
                     (do satisfy (=='+'); return Hepp
                      `onFail`
                      return Hep)
                  `onFail`
                  return He)
              `onFail`
              return H)

{-
slice :: Parser Char Slice
slice = do satisfy (=='x'); x <- (parse_range integer)
           satisfy (=='y'); y <- (parse_range integer)
           satisfy (=='z'); z <- (parse_range integer)
           satisfy (=='t'); t <- (parse_range integer)
           satisfy (=='.'); ss <- many1 species
           return Slice { x=x, y=y, z=z, t=t, s=ss }
-}

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

-- Low-level IO and conversion ----------------------------------------------

read_astro :: (Elt v, Ord v, Fractional v) => VisData -> Grid3D v
--read_astro :: VisData -> Grid3D Double
read_astro d@(From xr yr zr t sp)
    = let basename = show d
          summaryf = basename ++ ".summary"
          dim = Z :. range_size zr :. range_size yr :. range_size xr
          --get ix str = decode (toIndex dim ix) str
          -- vs = map (\i -> decode i str) [0..20]
          str = read_data $ basename ++ ".dat"
          -- arr = force $ fromFunction dim (\ix -> decode (toIndex dim ix) str)
          arr = fromList dim (bytesToValues str)                              
          (min, max) = fromMaybe (compute_bounds arr) (read_bounds summaryf)
      in    
          {-unsafePerformIO((putStrLn $ "Bounds: "++show min++" "++show max)  >>
                          (mapM_ print vs) >>
                          (print "---------")>>
                          (print $ arr !: (Z:.0:.0:.0)) >>
                          (print $ arr !: (Z:.0:.0:.1)) >>
                          (print $ arr !: (Z:.0:.0:.2))) `seq`-} 
          Grid basename (show sp) dim t min max arr
            
compute_bounds :: (Elt n, Ord n, Shape sh) => Array sh n -> (n, n)
compute_bounds arr = let v0 = arr !: (fromIndex (extent arr) 0)
                     in (foldAll min v0 arr, foldAll max v0 arr)
  
read_data :: String -> Fast.ByteString
read_data fnm = unsafePerformIO (            
                do h <- openFile fnm ReadMode 
                   Fast.hGetContents h)

bytesToValues :: Fractional a => Fast.ByteString -> [a]
bytesToValues bs
    | Fast.null bs   = []
    | otherwise      = fromRational val : bytesToValues post
                       where
                           (pre,post) = Fast.splitAt 4 bs
                           [a,b,c,d]  = Fast.unpack pre
                           exp = a16 .|. b16
                           man = c16 .|. d16
                           val | exp < 0   = toInteger man % (10 ^ negate (toInteger exp))
                               | otherwise = toInteger man * (10^exp) % 1    
                           a16 :: Int16 = fromIntegral a `shiftL` 8
                           b16 :: Int16 = fromIntegral b 
                           c16 :: Int16 = fromIntegral c `shiftL`  8
                           d16 :: Int16 = fromIntegral d
         
read_bounds fnm = unsafePerformIO $                   
                  do has_summary <- fileExist fnm
                     if has_summary
                       then do let vals = read_data fnm
                               let [min, max]  = bytesToValues vals
                               -- let max  = decode 1 vals
                               return $ Just (min,max)
                       else return Nothing

{-
decode :: Fractional v => Int -> Fast.ByteString -> v
--decode :: Int -> Fast.ByteString -> Double
decode i bs = let start {- :: Int64-} = fromIntegral $ i*4
                  a = bs `Fast.index` start
                  b = bs `Fast.index` (start + 1)
                  c = bs `Fast.index` (start + 2)
                  d = bs `Fast.index` (start + 3)
                  a32 :: Word32 = fromIntegral a `shiftL` 24
                  b32 :: Word32 = fromIntegral b `shiftL` 16
                  c32 :: Word32 = fromIntegral c `shiftL`  8
                  d32 :: Word32 = fromIntegral d
               in 
                  realToFrac . fromSample . Sample $ (a32 .|. b32 .|. c32 .|. d32)
-}
{-
bytesToSamples :: Fast.ByteString -> [Sample]
bytesToSamples bs 
    | Fast.null bs   = []
    | otherwise      = (Sample s):bytesToSamples post
                       where
                           (pre,post) = Fast.splitAt 4 bs
                           [a,b,c,d]  = Fast.unpack pre
                           s = (a32 .|. b32 .|. c32 .|. d32)
                           a32 :: Word32 = fromIntegral a `shiftL` 24
                           b32 :: Word32 = fromIntegral b `shiftL` 16
                           c32 :: Word32 = fromIntegral c `shiftL`  8
                           d32 :: Word32 = fromIntegral d

sampleToFloat :: Sample -> Float
sampleToFloat = realToFrac . fromSample
-}


