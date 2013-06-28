module FPProperties where

import Data.Int
import Data.Ratio
import Prelude hiding (exponent)
import FixedPrecision
import IntOps
import CFP
import Test.SmallCheck

instance Serial FixedPrecision where
  series d = [normaliseFP $ FP (fromIntegral m) (fromIntegral e) | (m,e) <- series d :: [(Int,Int)]]

-- Properties of operations on FixedPrecision values.

prop_norm :: FixedPrecision -> Bool
prop_norm x = normaliseFP x == x

prop_ordReflexive :: FixedPrecision -> Bool
prop_ordReflexive x = x <= x

prop_ordAntisymmetric :: FixedPrecision -> FixedPrecision -> Property
prop_ordAntisymmetric x y = x <= y && y <= x ==> x == y

prop_ordTransitive :: FixedPrecision -> FixedPrecision -> FixedPrecision -> Property
prop_ordTransitive x y z = x <= y && y <= z ==> x <= z

prop_eqReflexive :: FixedPrecision -> Bool
prop_eqReflexive x = x == x

prop_eqSymmetric :: FixedPrecision -> FixedPrecision -> Property
prop_eqSymmetric x y = x == y ==> y == x

prop_eqTransitive :: FixedPrecision -> FixedPrecision -> FixedPrecision -> Property
prop_eqTransitive x y z = x == y && y == z ==> x == z

prop_plusZero :: FixedPrecision -> Bool
prop_plusZero x = x + 0 == x

prop_plusCommutes :: FixedPrecision -> FixedPrecision -> Bool
prop_plusCommutes x y = x + y == y + x

prop_timesZero :: FixedPrecision -> Bool
prop_timesZero x = x * 0 == 0

prop_timesOne :: FixedPrecision -> Bool
prop_timesOne x = x * 1 == x

prop_timesCommutes :: FixedPrecision -> FixedPrecision -> Bool
prop_timesCommutes x y = x * y == y * x

prop_doubleAdd :: FixedPrecision -> Bool
prop_doubleAdd x = x + x == 2 * x

prop_distrib :: FixedPrecision -> FixedPrecision -> FixedPrecision -> Bool
prop_distrib x y z = x * (y + z) == (x*y) + (x*z)

prop_negatePlus :: FixedPrecision -> FixedPrecision -> Bool
prop_negatePlus x y = negate (x + y) == negate x + negate y

prop_negateMinus :: FixedPrecision -> FixedPrecision -> Bool
prop_negateMinus x y = negate (x - y) == y - x

prop_plusMinus :: FixedPrecision -> FixedPrecision -> Bool
prop_plusMinus x y = (x + y) - y == x

prop_recipDivide :: FixedPrecision -> FixedPrecision -> Property
prop_recipDivide x y = x /= 0 && y /= 0 ==> recip (x / y) == y / x

prop_timesDivide :: FixedPrecision -> FixedPrecision -> Property
prop_timesDivide x y = y /= 0 ==> (x * y) / y == x

prop_sqrootMinErr :: FixedPrecision -> Property
prop_sqrootMinErr x = x >= 0 ==> rdiff == minimum diffs
  where
  (FP rm re) = sqroot x
  diffs = [abs (x - y*y) | rm' <- [rm-1 .. rm+1], 0<=rm', rm'<=9999,
                           let y = (FP rm' re) ]
  rdiff =  abs (x - y*y) where y = (FP rm  re)

testFixedPrecision d = do
  putStrLn "norm"
  depthCheck d prop_norm
  putStrLn "ordReflexive"
  depthCheck d prop_ordReflexive
  putStrLn "ordAntisymmetric"
  depthCheck d prop_ordAntisymmetric
  putStrLn "ordTransitive"
  depthCheck (d-1) prop_ordTransitive
  putStrLn "eqReflexive"
  depthCheck d prop_eqReflexive
  putStrLn "eqSymmetric"
  depthCheck d prop_eqSymmetric
  putStrLn "eqTransitive"
  depthCheck (d-1) prop_eqTransitive
  putStrLn "plusZero"
  depthCheck d prop_plusZero
  putStrLn "plusCommutes"
  depthCheck d prop_plusCommutes
  putStrLn "timesZero"
  depthCheck d prop_timesZero
  putStrLn "timesOne"
  depthCheck d prop_timesOne
  putStrLn "timesCommutes"
  depthCheck d prop_timesCommutes
  putStrLn "doubleAdd"
  depthCheck d prop_doubleAdd
  putStrLn "negatePlus"
  depthCheck d prop_negatePlus
  putStrLn "negateMinus"
  depthCheck d prop_negateMinus
  putStrLn "plusMinus"
  depthCheck d prop_plusMinus
  putStrLn "recipDivide"
  depthCheck d prop_recipDivide
  putStrLn "timesDivide"
  depthCheck d prop_timesDivide
  putStrLn "sqrootSquare"
  depthCheck d prop_sqrootMinErr

-- Correspondence between CFP and FixedPrecision

toCFP :: FixedPrecision -> CFP
toCFP fp = sampleOf (fromIntegral $ mantissa fp) (fromIntegral $ exponent fp)

fromCFP :: CFP -> FixedPrecision
fromCFP cfp = FP (fromIntegral $ fixedMan cfp) (fromIntegral $ fixedExp cfp)

prop_toFrom :: FixedPrecision -> Bool
prop_toFrom x = fromCFP (toCFP x) == x

lift2 :: (CFP -> CFP -> CFP) -> FixedPrecision -> FixedPrecision -> FixedPrecision
lift2 op x y = fromCFP (toCFP x `op` toCFP y)

prop_plus :: FixedPrecision -> FixedPrecision -> Bool
prop_plus x y = (x + y) == fromCFP (toCFP x + toCFP y)

prop_minus :: FixedPrecision -> FixedPrecision -> Bool
prop_minus x y = (x - y) == fromCFP (toCFP x - toCFP y)

prop_times :: FixedPrecision -> FixedPrecision -> Bool
prop_times x y = (x * y) == fromCFP (toCFP x * toCFP y)

prop_divide :: FixedPrecision -> FixedPrecision -> Property
prop_divide x y = y /= 0 ==> (x / y) == fromCFP (toCFP x / toCFP y)

prop_recip :: FixedPrecision -> Property
prop_recip x = x /= 0 ==> recip x == fromCFP (fixedPrecRecip (toCFP x))

prop_sqroot :: FixedPrecision -> Property
prop_sqroot x = x >= 0 ==> sqroot x == fromCFP (fixedPrecSqroot (toCFP x))

prop_intSqroot :: Int -> Property
prop_intSqroot n = n >= 0 ==> intSqrootC n == intSqroot n
