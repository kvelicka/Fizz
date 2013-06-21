
module Vector_3

where

import Data.List (foldl')
import Control.Applicative

data V3 a = V3 !a !a !a deriving Show
type Vector3 = V3 Float

instance Functor V3 where
  fmap f (V3 a b c) = V3 (f a) (f b) (f c)
instance Applicative V3 where
  pure f = V3 f f f
  (V3 f1 f2 f3) <*> (V3 v1 v2 v3) = V3 (f1 v1) (f2 v2) (f3 v3)

vsum :: Vector3 -> Vector3 -> Vector3
vsum va vb = pure (+) <*> va <*> vb

diff :: Vector3 -> Vector3 -> Vector3
diff va vb = pure (-) <*> va <*> vb

scalar_vec :: Vector3 -> Float -> Vector3
scalar_vec va c = pure (*) <*> pure c <*> va

dot :: Vector3 -> Vector3 -> Vector3
dot va vb = pure (*) <*> va <*> vb

cross :: Vector3 -> Vector3 -> Vector3

cross (V3 i1 j1 k1) (V3 i2 j2 k2) = V3 (j1*k2 + k1*j2) (-(i1*k2  + k1*i2))  (i1*j2 + j1*i2)

squared_mag :: Vector3 -> Float
squared_mag (V3 i j k) = (i*i + j*j + k*k)

magnitude :: Vector3 -> Float
magnitude v = sqrt (squared_mag v)

normalize :: Vector3 -> Vector3
normalize v = let  m = (magnitude v) 
 	      in if m/=0  then scalar_vec v (1/m) else V3 0 0 0

test = foldl' (\a b -> normalize $ cross (vsum a b) (diff a b)) (V3 1 1 1) $ 
       take 1000000 (zipWith3 V3 [0..] [1..] [2..])
