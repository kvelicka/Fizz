
module Vector

where

import Data.List (foldl')
import Control.Applicative


class (Applicative f) => Vector f where
    vconst :: a -> f a
    vsum :: Num a => f a -> f a -> f a
    diff :: Num a => f a -> f a -> f a
    scalar_vec :: Num a => f a -> a -> f a
    dot :: Num a => f a -> f a ->f  a
    cross :: Num a => f a -> f a -> f a
    squared_mag :: RealFloat a => f a -> a



magnitude :: (Vector f) => f Float -> Float
magnitude v = sqrt (squared_mag v)

normalize :: (Vector f) => f Float -> f Float
normalize v = let  m = (magnitude v) 
              in if m/=0  then scalar_vec v (1/m) else vconst 0

data V3 a = V3 !a !a !a deriving Show
type Vector3 = V3 Float

instance Functor V3 where
  fmap f (V3 a b c) = V3 (f a) (f b) (f c)
instance Applicative V3 where
  pure f = V3 f f f
  (V3 f1 f2 f3) <*> (V3 v1 v2 v3) = V3 (f1 v1) (f2 v2) (f3 v3)

instance Vector V3 where
    vconst c = V3 c c c
    squared_mag (V3 a b c) = (a*a + b*b + c*c)
    vsum va vb = pure (+) <*> va <*> vb
    diff va vb = pure (-) <*> va <*> vb
    scalar_vec va c = pure (*) <*> pure c <*> va
    dot va vb = pure (*) <*> va <*> vb
    cross (V3 i1 j1 k1) (V3 i2 j2 k2) = V3 (j1*k2 + k1*j2) (-(i1*k2  + k1*i2))  (i1*j2 + j1*i2)

_vsum :: V3 Float -> V3 Float -> V3 Float
_vsum = vsum

_diff :: V3 Float -> V3 Float -> V3 Float
_diff = diff

_cross :: V3 Float -> V3 Float -> V3 Float
_cross = cross


test = foldl' (\a b -> normalize $ _cross (_vsum a b) (_diff a b)) (V3 1 1 1) $ 
       take 1000000 (zipWith3 V3 [0..] [1..] [2..])

