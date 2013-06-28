{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, PackageImports, BangPatterns #-}

{- Interface to datasets from the IEEE Visualization contest 2008.
   This file implements a set of interfaces for reading, accessing
   and manipulating these data.
-}

module AstroData where

import Data.Bits
import Data.Char
import Data.Maybe
import Data.Ratio
import Data.Word
import GHC.Int
import Numeric
import qualified Data.ByteString as BS
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files
import Text.ParserCombinators.Poly

import Dataset
import FixedPrecision
import Sample hiding (s)

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

type Time = Int

data VisData = From (Sampling Int) (Sampling Int) (Sampling Int) Time Species
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

parse_range :: Num a => Parser Char a -> Parser Char (Sampling a)
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

read_astro_file :: String -> IO (Grid DIM3 Float)
read_astro_file str
    = read_astro_data (result . fst . runParser slice $ str)
      where result (Left err) = error err       
            result (Right ok) = ok

read_astro_data :: VisData -> IO (Grid DIM3 Float)
read_astro_data d@(From xr yr zr t s)
    = do { let basename = show d
         ; let summaryf = basename ++ ".summary"
         ; let dim = Z :. range_size zr :. range_size yr :. range_size xr
         ; h <- openFile (basename ++ ".dat") ReadMode 
         ; b <- BS.hGetContents h
         ; hs <- openFile summaryf ReadMode
         ; bs <- BS.hGetContents hs
         ; let [minv,maxv] = map sampleToFloat . bytesToSamples $ bs
         ; return $ Grid basename (show s) dim t (Exact minv maxv) (Values (xr,yr,zr) b $ bytesToFloats b)
         }
  
bytesToValues :: Fractional a => Int -> BS.ByteString -> a
bytesToValues !i bs
    = let start = fromIntegral $ i*4
          a = bs `BS.index` start
          b = bs `BS.index` (start + 1)
          c = bs `BS.index` (start + 2)
          d = bs `BS.index` (start + 3)
          a16 :: Int16 = fromIntegral a `shiftL` 8
          b16 :: Int16 = fromIntegral b 
          c16 :: Int16 = fromIntegral c `shiftL`  8
          d16 :: Int16 = fromIntegral d    
          exp = a16 .|. b16
          man = c16 .|. d16
          val | exp < 0   = toInteger man % (10 ^ negate (toInteger exp))
              | otherwise = toInteger man * (10^exp) % 1  
      in realToFrac val


bytesToFloats :: BS.ByteString -> [Float]
bytesToFloats bs 
    | BS.null bs   = []
    | otherwise      = (sampleToFloat $ Sample s):bytesToFloats post
                       where
                           (pre,post) = BS.splitAt 4 bs
                           [a,b,c,d]  = BS.unpack pre
                           s = (a32 .|. b32 .|. c32 .|. d32)
                           a32 :: Word32 = fromIntegral a `shiftL` 24
                           b32 :: Word32 = fromIntegral b `shiftL` 16
                           c32 :: Word32 = fromIntegral c `shiftL`  8
                           d32 :: Word32 = fromIntegral d