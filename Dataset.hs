{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeOperators, EmptyDataDecls #-}

module Dataset where

import qualified Data.ByteString as BS

class Dataset d t where
  resource :: d -> String
  read_data :: d -> IO (Grid DIM3 t)

-- Datatypes to define dimensions
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

-- Sampling datatype, that may be used to downsample datasets or take slices
data Sampling a = Single  a
                | Range   a a
                | Sampled a a a
                  deriving Eq

instance (Show a, Num a) => Show (Sampling a) where
  show (Single  i)     = show i
  show (Range   i j)   = show i ++"-"++show j
  show (Sampled i j k) = show i ++"-"++show (i+j)++"-"++show k


range_to_list :: (Num a, Enum a) => Sampling a -> [a]
range_to_list (Single i)      = [i]
range_to_list (Range f t)     = [f..t]
range_to_list (Sampled f s t) = [f,s+f .. t]

range_size :: Integral a => Sampling a -> a
range_size (Single _)      = 1
range_size (Range f t)     = t - f + 1
range_size (Sampled f s t) = ((t - f) `div` s) + 1


-- Container for sampling and values
data Values = Values { dimensions :: (Sampling Int, Sampling Int, Sampling Int)
                     , datastream :: BS.ByteString
                     , extract    :: [Float]
                     } deriving Show

-- Datatype to bound the magnitude of values in the dataset
data MinMaxValues a = Unknown 
                    | Bounded a a 
                    | Exact a a
                      deriving Show

-- A generic grid to accomodate various datasets
data Grid sh v = Grid { origin  :: String
                      , field   :: String
                      , shape   :: sh
                      , time    :: Int
                      , min_max :: MinMaxValues v
                      , values  :: Values
                      } deriving Show

type Grid2D a = Grid DIM2 a
type Grid3D a = Grid DIM3 a

