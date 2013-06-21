
module Vector_1

where

import Data.List (foldl')

type Vector3 = (!Float, !Float, !Float)

vsum :: Vector3 -> Vector3 -> Vector3
vsum (i1, j1, k1) (i2, j2, k2) = (i1+i2, j1+j2, k1+k2)


diff :: Vector3 -> Vector3 -> Vector3
diff (i1, j1, k1) (i2, j2, k2) = (i1-i2, j1-j2, k1-k2)

scalar_vec :: Vector3 -> Float -> Vector3
scalar_vec (i1, j1, k1) c = (i1*c, j1*c, k1*c)

dot :: Vector3 -> Vector3 -> Vector3
dot (i1, j1, k1) (i2, j2, k2) = (i1*i2, j1*j2, k1*k2)

cross :: Vector3 -> Vector3 -> Vector3
cross (i1, j1, k1) (i2, j2, k2) = (j1*k2 + k1*j2, -(i1*k2  + k1*i2), i1*j2 + j1*i2)

squared_mag :: Vector3 -> Float
squared_mag (i, j, k) = (i*i + j*j + k*k)

magnitude :: Vector3 -> Float
magnitude v = sqrt (squared_mag v)

normalize :: Vector3 -> Vector3
normalize v = let  m = (magnitude v) 
 	      in if m/=0  then scalar_vec v (1/m) else (0,0,0)

test = foldl' (\a b -> normalize $ cross (vsum a b) (diff a b)) (1,1,1) $ 
       take 1000000 (zip3 [0..] [1..] [2..])
