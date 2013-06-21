
module Vector_5

where

import Data.List (foldl')
import Data.Packed.Vector

type Vector3 = Vector Double


vsum :: Vector3 -> Vector3 -> Vector3
vsum = liftVector2 (+)


diff :: Vector3 -> Vector3 -> Vector3
diff = liftVector2 (-)

scalar_vec :: Vector3 -> Double -> Vector3
scalar_vec v c = liftVector (*c) v

dot :: Vector3 -> Vector3 -> Vector3
dot = liftVector2 (*)

cross :: Vector3 -> Vector3 -> Vector3
cross va vb = let i1 = va @> 0
                  j1 = va @> 1
                  k1 = va @> 2
                  i2 = vb @> 0
                  j2 = vb @> 1
                  k2 = vb @> 2
              in 3 |> [j1*k2 + k1*j2, -(i1*k2  + k1*i2), i1*j2 + j1*i2]

squared_mag :: Vector3 -> Double
squared_mag v = sum . toList $ v `dot` v

magnitude :: Vector3 -> Double
magnitude v = sqrt (squared_mag v)

normalize :: Vector3 -> Vector3
normalize v = let  m = (magnitude v) 
 	      in if m/=0  then scalar_vec v (1/m) else (constant 0 3)

test = toList $
       foldl' (\a b -> normalize $ cross (vsum a b) (diff a b)) (constant 1 3) $ 
       take 1000000 (zipWith3 (\a b c -> 3 |> [a,b,c]) [0..] [1..] [2..])
