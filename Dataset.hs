{-# LANGUAGE FlexibleInstances, TypeOperators #-}

module Dataset where

import Data.Array.Repa hiding ( (++) )
import Data.Array.Repa.IO.Binary
import Data.Array.Repa.Repr.ForeignPtr
import qualified Graphics.Rendering.OpenGL.GL as GL
import System.IO (openBinaryFile, IOMode(ReadMode), hFileSize, hClose)

import Maths


-- Generic dataset class. Used to convert specific data types into internal
-- FizzData representation.
class Dataset d where
  readData  :: d -> IO (FizzData Float)


-- Sampling datatype - used to downsample datasets.
data Sampling a = Single  a
                | Range   a a
                | Sampled a a a
                  deriving Eq

type Samplings = (Sampling Int, Sampling Int, Sampling Int)

instance (Show a, Num a) => Show (Sampling a) where
  show (Single  i)     = show i
  show (Range   i j)   = show i ++ "-"++ show j
  show (Sampled i j k) = show i ++ "-"++ show (i+j) ++ "-" ++ show k

samplingToList :: (Num a, Enum a) => Sampling a -> [a]
samplingToList (Single i)      = [i]
samplingToList (Range f t)     = [f..t]
samplingToList (Sampled f s t) = [f,s+f .. t]

samplingSize :: (Integral a) => Sampling a -> a
samplingSize (Single _)      = 1
samplingSize (Range f t)     = t - f + 1
samplingSize (Sampled f s t) = ((t - f) `div` s) + 1

listX d = let (dx, _, _) = d in samplingToList dx
listY d = let (_, dy, _) = d in samplingToList dy
listZ d = let (_, _, dz) = d in samplingToList dz

dimensions :: Samplings -> (Int, Int, Int)
dimensions (x, y, z) = (samplingSize x, samplingSize y, samplingSize z)

dimensions2D :: Samplings -> (Int, Int)
dimensions2D (x, y, _) = (samplingSize x, samplingSize y)

-- A datatype that is used intenrally to convert datasets to pictures.
data FizzData v = FizzData { origin     :: String
                           , samplings  :: Samplings
                           , datastream :: [Float]
                           --, arr       :: Array F DIM1 v
                           } -- deriving Show

fileSize :: FilePath -> IO Int
fileSize path = do
  h <- openBinaryFile path ReadMode 
  sz <- hFileSize h
  hClose h
  return $ fromIntegral sz

data Plane = X_equals Int 
           | Y_equals Int 
           | Z_equals Int 
             deriving (Eq, Show)

slicePlane :: Samplings -> Plane
slicePlane (r, _, _) | isSingleton r = X_equals . head . samplingToList $ r
slicePlane (_, r, _) | isSingleton r = Y_equals . head . samplingToList $ r
slicePlane (_, _, r) | isSingleton r = Z_equals . head . samplingToList $ r
slicePlane _                           = error "slicePlane: no singleton dimension"

isSingleton :: Ord a => Sampling a -> Bool 
isSingleton (Single _)      = True
isSingleton (Range f t)     = f == t
isSingleton (Sampled f s t) = f == t || f == s || s > t

planePoints :: Samplings -> [GL.Vertex3 GL.GLfloat]
planePoints samp =
    case slicePlane samp of
      X_equals v -> [coord v cy cz | cz <- samplingZ, cy <- samplingY]
      Y_equals v -> [coord cx v cz | cz <- samplingZ, cx <- samplingX]
      Z_equals v -> [coord cx cy v | cy <- samplingY, cx <- samplingX]
    where
      samplingZ = listZ $ samp
      samplingY = listY $ samp
      samplingX = listX $ samp
      coord x y z = GL.Vertex3 (toFloat x) (toFloat y) (toFloat z)
