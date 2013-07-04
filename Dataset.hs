{-# LANGUAGE ExistentialQuantification, FunctionalDependencies, FlexibleInstances, TypeOperators, EmptyDataDecls #-}

module Dataset where

import qualified Data.ByteString as BS


-- Generic dataset class. Used to convert specific data types into internal
-- FizzData representation.
class Dataset d where
  readData :: d -> IO (FizzData3D a)

data Source = forall d. Dataset d => Source d

-- Datatypes to define dimensions of FizzData
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

dim_x, dim_y, dim_z :: Dim d => d -> Int
dim_x d = let [_,_,x] = dims d in x
dim_y d = let [_,y,_] = dims d in y
dim_z d = let [z,_,_] = dims d in z

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

{- REASON: Not used in the current data represeantation
-- Datatype to bound the magnitude of values in the dataset
data MinMaxValues a = Unknown
                    | Bounded a a
                    | Exact a a
                      deriving Show
-}

-- A datatype that is used intenrally to convert datasets to pictures.
data FizzData sh v = FizzData { origin     :: String
                              , shape      :: sh
                              , raw        :: BS.ByteString
                              , stream     :: [Float]
                              } deriving Show

dimensions :: Dim sh => FizzData sh v -> (Int, Int, Int)
dimensions d = let [dx,dy,dz] = dims . shape $ d in (dx, dy, dz)


type FizzData2D a = FizzData DIM2 a
type FizzData3D a = FizzData DIM3 a

type Context a = [FizzData3D a]
