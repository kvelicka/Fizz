
module Vector_2

where

import Data.List (foldl')

data Vector3 = V3 !Float !Float !Float deriving Show

vsum :: Vector3 -> Vector3 -> Vector3
vsum (V3 i1 j1 k1) (V3 i2 j2 k2) = V3 (i1+i2) (j1+j2) (k1+k2)


diff :: Vector3 -> Vector3 -> Vector3
diff (V3 i1 j1 k1) (V3 i2 j2 k2) = V3 (i1-i2) (j1-j2) (k1-k2)

scalar_vec :: Vector3 -> Float -> Vector3
scalar_vec (V3 i1 j1 k1) c = V3 (i1*c) (j1*c) (k1*c)

dot :: Vector3 -> Vector3 -> Vector3
dot (V3 i1 j1 k1) (V3 i2 j2 k2) = V3 (i1*i2) (j1*j2) (k1*k2)

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
