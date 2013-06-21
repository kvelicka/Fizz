{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeOperators, EmptyDataDecls #-}

module Dataset where

import Data.Array.Repa

class Dataset d t where
  resource :: d -> String
  read_data :: d -> IO (Grid DIM3 t)


data Grid sh v = Grid { origin  :: String
                      , field   :: String
                      , shape   :: sh
                      , time    :: Int
                      , minv    :: v
                      , maxv    :: v
                      , values  :: Array U DIM1 Float
                      } deriving Show

type Grid3D a = Grid DIM3 a
type Grid2D a = Grid DIM2 a


dim_x, dim_y, dim_z :: Shape d => d -> Int
dim_x d = let [_,_,x] = listOfShape d in x
dim_y d = let [_,y,_] = listOfShape d in y 
dim_z d = let [z,_,_] = listOfShape d in z



