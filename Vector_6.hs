
module Vector_6

where

import Data.List (foldl')
import Control.Applicative

data V3 a = V3 ![a] deriving Show
type Vector3 = V3 Float

instance Functor V3 where
  fmap f (V3 vs) = V3 (map f vs)
instance Applicative V3 where
  pure f = V3 (replicate 3 f)
  (V3 fa) <*> (V3 fb) = V3 $ zipWith ($) fa fb

vsum :: Vector3 -> Vector3 -> Vector3
-- vsum va vb = pure (+) <*> va <*> vb
vsum (V3 as) (V3 bs) = V3 (zipWith (+) as bs)

diff :: Vector3 -> Vector3 -> Vector3
-- diff va vb = pure (-) <*> va <*> vb
diff (V3 as) (V3 bs) = V3 (zipWith (-) as bs)

scalar_vec :: Vector3 -> Float -> Vector3
-- scalar_vec va c = pure (*) <*> pure c <*> va
scalar_vec (V3 as) c = V3 $ map (*c) as

dot :: Vector3 -> Vector3 -> Vector3
--dot va vb = pure (*) <*> va <*> vb
dot (V3 as) (V3 bs) = V3 $ zipWith (*) as bs

cross :: Vector3 -> Vector3 -> Vector3

cross (V3 [i1,j1,k1]) (V3 [i2,j2,k2])
    = V3 [j1*k2 + k1*j2, (-(i1*k2  + k1*i2)),  (i1*j2 + j1*i2)]

squared_mag :: Vector3 -> Float
squared_mag (V3 vs) = sum $ zipWith (*) vs vs

magnitude :: Vector3 -> Float
magnitude v = sqrt (squared_mag v)

normalize :: Vector3 -> Vector3
normalize v = let  m = (magnitude v) 
 	      in if m/=0  then scalar_vec v (1/m) else V3 [ 0, 0, 0]

test = foldl' (\a b -> normalize $ cross (vsum a b) (diff a b)) (V3 [1, 1, 1]) $ 
       take 1000000 (zipWith3 (\a b c -> V3 [a,b,c]) [0..] [1..] [2..])
