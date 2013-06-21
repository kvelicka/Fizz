{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, PackageImports, BangPatterns #-}

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
import qualified Data.Array.Parallel.Unlifted as U

import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

import System.IO
import System.Posix.Files
import Data.Bits
import FixedPrecision
import Sample hiding (s)
import Data.Word
import qualified Data.ByteString {-.Lazy -} as Fast
import GHC.Int
import Data.Ratio
import Numeric


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

-- Low-level IO and conversion ----------------------------------------------

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
            
{-
read_astro :: (U.Elt v, Ord v, Fractional v) => VisData -> IO (Grid3D v)
read_astro d@(From xr yr zr t sp)
    = do let basename = show d
         let summary = basename ++ ".summary"
         let dim = Z :. range_size zr :. range_size yr :. range_size xr
         str <- Fast.readFile $ basename ++ ".dat"
         let arr = fromList dim $ bytesToValues str
         haveSummary <- fileExist summary    
         [min, max] <- if haveSummary
                          then return . bytesToValues =<< Fast.readFile summary
                          else return $ compute_bounds arr
         return $ Grid basename (show sp) dim t min max arr
-}

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

{-
bytesToValues :: Fractional a => Int -> Fast.ByteString -> IO (U.Array a)
bytesToValues size bs
    = do US.newU size $ \(arr :: U.Array Double) -> let loop !i | i >= size = return arr
                                                                | otherwise = do let start = i*4
                                                                                 let a = bs `Fast.index` start
                                                                                 let b = bs `Fast.index` (start + 1)
                                                                                 let c = bs `Fast.index` (start + 2)
                                                                                 let d = bs `Fast.index` (start + 3)
                                                                                 let a16 :: Int16 = fromIntegral a `shiftL` 8
                                                                                 let b16 :: Int16 = fromIntegral b 
                                                                                 let c16 :: Int16 = fromIntegral c `shiftL`  8
                                                                                 let d16 :: Int16 = fromIntegral d    
                                                                                 let exp = a16 .|. b16
                                                                                 let man = c16 .|. d16
                                                                                 let val | exp < 0   = toInteger man % (10 ^ negate (toInteger exp))
                                                                                         | otherwise = toInteger man * (10^exp) % 1  
                                                                                 US.unsafeWrite i val arr
                                                                                 loop (i+1) 
                                                    in do loop 0 
                                                          US.unsafeFreezeAllMU arr
                                                          return arr
-}

{-         
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
-}
