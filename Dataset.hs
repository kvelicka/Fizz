{-# LANGUAGE ExistentialQuantification, FunctionalDependencies, FlexibleInstances, TypeOperators, EmptyDataDecls #-}

module Dataset where

import qualified Data.ByteString as BS


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

class Dim d where
  size      :: d -> Int
  dims      :: d -> [Int]
  samplings :: d -> [Sampling Int]

instance Dim Z where
  size      = const 0
  dims      = const []
  samplings = const []

instance (Dim a) => Dim (a :. Sampling Int) where
  size (a :. b)      = size a * samplingSize b
  dims (a :. b)      = dims a ++ [samplingSize b]
  samplings (a :. b) = samplings a ++ [b]

data Z = Z
data a :. b = a :. b

type DIM0 = Z
type DIM1 = DIM0 :. Sampling Int
type DIM2 = DIM1 :. Sampling Int
type DIM3 = DIM2 :. Sampling Int

sizeX d = let [dx, _, _] = dims d in dx
sizeY d = let [_, dy, _] = dims d in dy
sizeZ d = let [_, _, dz] = dims d in dz

dimensions :: (Dim sh) => sh -> (Int, Int, Int)
dimensions d = (sizeX d, sizeY d, sizeZ d)

listX d = let [dx, _, _] = samplings d in samplingToList dx
listY d = let [_, dy, _] = samplings d in samplingToList dy
listZ d = let [_, _, dz] = samplings d in samplingToList dz

sizeX2D d = let [dx, _] = dims d in dx
sizeY2D d = let [_, dy] = dims d in dy

dimensions2D :: (Dim sh) => sh -> (Int, Int)
dimensions2D d = (sizeX d, sizeY d)

listX2D d = let [dx, _] = samplings d in samplingToList dx
listY2D d = let [_, dy] = samplings d in samplingToList dy

-- A datatype that is used intenrally to convert datasets to pictures.
data FizzData sh v = FizzData { origin     :: String
                              , shape      :: sh
                              , raw        :: BS.ByteString
                              , stream     :: [Float]
                              } deriving Show

type FizzData2D a = FizzData DIM2 a
type FizzData3D a = FizzData DIM3 a

type Context a = [FizzData3D a]
