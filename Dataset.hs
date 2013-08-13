{-# LANGUAGE FlexibleInstances, TypeOperators #-}

module Dataset where

import qualified Data.ByteString as BS
import qualified Graphics.Rendering.OpenGL.GL as GL

import Maths


-- Generic dataset class. Used to convert specific data types into internal
-- FizzData representation.
class Dataset d where
  readData  :: d -> IO (FizzData3D a)


-- Sampling datatype - used to downsample datasets.
data Sampling a = Single  a
                | Range   a a
                | Sampled a a a
                  deriving Eq

instance (Show a, Num a) => Show (Sampling a) where
  show (Single  i)     = show i
  show (Range   i j)   = show i ++"-"++show j
  show (Sampled i j k) = show i ++"-"++show (i+j)++"-"++show k

samplingToList :: (Num a, Enum a) => Sampling a -> [a]
samplingToList (Single i)      = [i]
samplingToList (Range f t)     = [f..t]
samplingToList (Sampled f s t) = [f,s+f .. t]

samplingSize :: (Integral a) => Sampling a -> a
samplingSize (Single _)      = 1
samplingSize (Range f t)     = t - f + 1
samplingSize (Sampled f s t) = ((t - f) `div` s) + 1


class Dim sh where
  size      :: sh -> Int
  dims      :: sh -> [Int]
  samplings :: sh -> [Sampling Int]

instance Dim Z where
  size      = const 0
  dims      = const []
  samplings = const []

instance (Dim a) => Dim (a :. Sampling Int) where
  size (a :. b)      = size a * samplingSize b
  dims (a :. b)      = dims a ++ [samplingSize b]
  samplings (a :. b) = samplings a ++ [b]

data Z = Z
infixl 3 :.
data a :. b = a :. b

type DIM0 = Z
type DIM1 = DIM0 :. Sampling Int
type DIM2 = DIM1 :. Sampling Int
type DIM3 = DIM2 :. Sampling Int

sizeX d = let [dx, _, _] = dims d in dx
sizeY d = let [_, dy, _] = dims d in dy
sizeZ d = let [_, _, dz] = dims d in dz

listX d = let [dx, _, _] = samplings d in samplingToList dx
listY d = let [_, dy, _] = samplings d in samplingToList dy
listZ d = let [_, _, dz] = samplings d in samplingToList dz

dimensions :: (Dim sh) => sh -> (Int, Int, Int)
dimensions d = (sizeX d, sizeY d, sizeZ d)

dimensions2D :: (Dim sh) => sh -> (Int, Int)
dimensions2D d = (sizeX d, sizeY d)

-- A datatype that is used intenrally to convert datasets to pictures.
data FizzData sh v = FizzData { origin     :: String
                              , shape      :: sh
                              , raw        :: BS.ByteString
                              , datastream :: [Float]
                              } deriving Show

type FizzData2D a = FizzData DIM2 a
type FizzData3D a = FizzData DIM3 a


data Plane = X_equals Int 
           | Y_equals Int 
           | Z_equals Int 
             deriving (Eq, Show)

slicePlane :: DIM3 -> Plane
slicePlane (Z :. r :. _ :. _) | isSingleton r = X_equals . head . samplingToList $ r
slicePlane (Z :. _ :. r :. _) | isSingleton r = Y_equals . head . samplingToList $ r
slicePlane (Z :. _ :. _ :. r) | isSingleton r = Z_equals . head . samplingToList $ r
slicePlane _                           = error "slicePlane: no singleton dimension"

isSingleton :: Ord a => Sampling a -> Bool 
isSingleton (Single _)      = True
isSingleton (Range f t)     = f == t
isSingleton (Sampled f s t) = f == t || f == s || s > t

planePoints :: DIM3 -> [GL.Vertex3 GL.GLfloat]
planePoints sh =
    case slicePlane sh of
      X_equals v -> [coord v cy cz | cz <- samplingZ, cy <- samplingY]
      Y_equals v -> [coord cx v cz | cz <- samplingZ, cx <- samplingX]
      Z_equals v -> [coord cx cy v | cy <- samplingY, cx <- samplingX]
    where
      samplingZ = listZ $ sh
      samplingY = listY $ sh
      samplingX = listX $ sh
      coord x y z = GL.Vertex3 (toFloat x) (toFloat y) (toFloat z)
