module Main where

import Prelude hiding (splitAt)
import System.IO
import System.Environment

import Data.List (foldl',replicate)
import qualified Data.ByteString.Lazy as Fast
import Text.ParserCombinators.Poly.Lazy
import Sample
import FixedPrecision
import Data.Word
import Data.Bits
import Data.Char

import System.IO.Unsafe

-- Parsers
prange :: Parser Char (Int, Int, Int)
prange = do i <- integer
            (do satisfy (=='-')
                j <- integer
                (do satisfy (=='-')
                    k <- integer
                    return (i, (j-i), k)
                 `onFail`
                 return (i, i+1, j))
             `onFail`
             return (i,i,i))

integer :: Parser Char Int	-- positive only
integer = do cs <- many1 (satisfy isDigit)
             return (foldl1 (\n d-> n*10+d)
                            (map digitToInt cs))


-- File input

read_data :: String -> IO [Sample]
read_data fname
    = do { h <- openFile fname ReadMode 
         ; b <- Fast.hGetContents h
         ; return $ bytesToSamples b
         }

bytesToSamples :: Fast.ByteString -> [Sample]
bytesToSamples bs 
    | Fast.null bs   = []
    | otherwise      = (Sample s):bytesToSamples post
                       where
                           (pre,post) = Fast.splitAt 4 bs
                           [a,b,c,d]  = Fast.unpack pre
                           -- [d,c,b,a]  = Fast.unpack pre
                           s = (a32 .|. b32 .|. c32 .|. d32)
                           a32 :: Word32 = fromIntegral a `shiftL` 24
                           b32 :: Word32 = fromIntegral b `shiftL` 16
                           c32 :: Word32 = fromIntegral c `shiftL`  8
                           d32 :: Word32 = fromIntegral d

sampleToBytes :: Sample -> [Word8]
sampleToBytes (Sample s) = [d,c,b,a]
                           where
                               a :: Word8 = fromIntegral $ s
                               b :: Word8 = fromIntegral $ s `shiftR` 8
                               c :: Word8 = fromIntegral $ s `shiftR` 16
                               d :: Word8 = fromIntegral $ s `shiftR` 24

-- main: xdim ydim zdim tdim attr
main :: IO ()
main = do { [xdim,ydim,zdim,tdim,attr] <- getArgs
          ; let base = concat ["x",xdim,"y",ydim,"z",zdim]
          ; let (t0,ti,t1) = fst . (runParser prange) $ tdim
          ; let (x0,x1,xn) = fst . (runParser prange) $ xdim
          ; let xsz = ((xn-x0) `div` (x1-x0)) + 1
          ; h <- openFile (base++"t"++tdim++"."++attr++"_sum"++".dat") WriteMode
          -- ; putStrLn $ (show t0)++"\t"++(show ti)++"\t"++(show t1)
          ; mapM_ (summarise h xsz base attr) [t0,t0+ti .. t1]
          ; hClose h
          }
       where
          dim s = let (v0,v1,vn) = fst . (runParser prange) $ s in ((vn - v0) `div` v1) + 1

splitAt :: Int -> [a] -> [[a]]
splitAt _ [] = []
splitAt n vs = (take n vs): splitAt n (drop n vs)

summarise :: Handle -> Int -> String -> String -> Int -> IO()
summarise h xsz base attr t
    = do { ss <- read_data (base++"t"++(show t)++"."++attr++".dat")
         ; let fpvs = map fromSample ss
         ; let sums = foldl' (zipWith (+)) (replicate xsz (FP 0 0)) (splitAt xsz fpvs)
         ; mapM_ outsample (zip [0..] sums)
         }
      where
         outsample (x,v) = do { writeSample h (toSample.fromInteger.toInteger $ t)
                              ; writeSample h (toSample.fromInteger.toInteger $ x)
                              ; writeSample h (toSample v)
                              }
         writeSample h v = Fast.hPut h $ Fast.pack $ sampleToBytes v
