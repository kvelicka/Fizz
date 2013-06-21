{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeOperators, EmptyDataDecls #-}

module Dataset where

import qualified Data.ByteString as BS

class Dataset d t where
  resource :: d -> String
  read_data :: d -> IO (Grid DIM3 t)

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

data Source = Bytes BS.ByteString | Samples [Float] deriving Show

data Grid sh v = Grid { origin  :: String
                      , field   :: String
                      , shape   :: sh
                      , time    :: Int
                      , minv    :: v
                      , maxv    :: v
                      , values  :: Source
                      } deriving Show

type Grid3D a = Grid DIM3 a
type Grid2D a = Grid DIM2 a

