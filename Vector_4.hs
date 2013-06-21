
module Vector_4

where

import Data.List (foldl')
import Data.Array.Vector

--data V3 a = V3 !a !a !a deriving Show
type Vector3 = UArr Float

vsum :: Vector3 -> Vector3 -> Vector3
vsum = zipWithU (+)

diff :: Vector3 -> Vector3 -> Vector3
diff = zipWithU (-)

scalar_vec :: Vector3 -> Float -> Vector3
scalar_vec v c = mapU (*c) v

dot :: Vector3 -> Vector3 -> Vector3
dot = zipWithU (*) 

cross :: Vector3 -> Vector3 -> Vector3
cross va vb 
    = let [i1,j1,k1] = fromU va
          [i2,j2,k2] = fromU vb
      in toU [(j1*k2 + k1*j2), (-(i1*k2  + k1*i2)),  (i1*j2 + j1*i2)]

squared_mag :: Vector3 -> Float
squared_mag va = sumU $ va `dot` va

magnitude :: Vector3 -> Float
magnitude v = sqrt (squared_mag v)

normalize :: Vector3 -> Vector3
normalize v = let  m = (magnitude v) 
 	      in if m/=0  then scalar_vec v (1/m) else toU [0,0,0]

test = foldl' (\a b -> normalize $ cross (vsum a b) (diff a b)) (toU [1, 1, 1]) $ 
       take 1000000 (zipWith3U (\a b c -> a `consU` b `consU` c `consU` emptyU) (toU [0..])  (toU [1..]) (toU [2..]))
