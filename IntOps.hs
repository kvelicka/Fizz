module IntOps where

import Test.SmallCheck

-- Closest integer approximation to square root
-- (could also compute lower bound too)
intSqroot :: Int -> Int
intSqroot i = intSqroot' 0 (rootBound i)
  where
  -- invariant: lo*lo <= i && i <= hi*hi
  intSqroot' lo hi | hi == lo   = hi
                   | hi == lo+1 = case compare (i-lo*lo) (hi*hi-i) of
                                  LT -> lo
                                  EQ -> lo
                                  GT -> hi
                   | otherwise  = let mid = (lo+hi) `div` 2 in
                                  case compare (mid*mid) i of
                                  LT -> intSqroot' mid hi
                                  EQ -> mid
                                  GT -> intSqroot' lo mid

rootBound i = if i < 100 then smallRootBound i else 10 * rootBound ((i `div` 100) + 1)
  where
  smallRootBound i = length (takeWhile (i>) [d*d | d <- [0..9]])

propIntSqrootMinErr :: Int -> Bool
propIntSqrootMinErr i =
  rdiff == minimum diffs
  where
  r     = intSqroot i
  diffs = [abs (i - j*j) | j <- [r-1 .. r+1]]
  rdiff =  abs (i - r*r)

propRootBound :: Int -> Bool
propRootBound i = rb * rb >= i
  where
  rb = rootBound i
