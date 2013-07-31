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

propNorm :: FixedPrecision -> Bool
propNorm x = normaliseFP x == x

propOrdReflexive :: FixedPrecision -> Bool
propOrdReflexive x = x <= x

propOrdAntisymmetric :: FixedPrecision -> FixedPrecision -> Property
propOrdAntisymmetric x y = x <= y && y <= x ==> x == y

propOrdTransitive :: FixedPrecision -> FixedPrecision -> FixedPrecision -> Property
propOrdTransitive x y z = x <= y && y <= z ==> x <= z

propEqReflexive :: FixedPrecision -> Bool
propEqReflexive x = x == x

propEqSymmetric :: FixedPrecision -> FixedPrecision -> Property
propEqSymmetric x y = x == y ==> y == x

propEqTransitive :: FixedPrecision -> FixedPrecision -> FixedPrecision -> Property
propEqTransitive x y z = x == y && y == z ==> x == z

propPlusZero :: FixedPrecision -> Bool
propPlusZero x = x + 0 == x

propPlusCommutes :: FixedPrecision -> FixedPrecision -> Bool
propPlusCommutes x y = x + y == y + x

propTimesZero x = x * 0 == 0

propTimesOne :: FixedPrecision -> Bool
propTimesOne x = x * 1 == x

propTimesCommutes :: FixedPrecision -> FixedPrecision -> Bool
propTimesCommutes x y = x * y == y * x

propDoubleAdd :: FixedPrecision -> Bool
propDoubleAdd x = x + x == 2 * x

propDistrib :: FixedPrecision -> FixedPrecision -> FixedPrecision -> Bool
propDistrib x y z = x * (y + z) == (x*y) + (x*z)

propNegatePlus :: FixedPrecision -> FixedPrecision -> Bool
propNegatePlus x y = negate (x + y) == negate x + negate y

propNegateMinus :: FixedPrecision -> FixedPrecision -> Bool
propNegateMinus x y = negate (x - y) == y - x

propPlusMinus :: FixedPrecision -> FixedPrecision -> Bool
propPlusMinus x y = (x + y) - y == x

propRecipDivide :: FixedPrecision -> FixedPrecision -> Property
propRecipDivide x y = x /= 0 && y /= 0 ==> recip (x / y) == y / x

propTimesDivide :: FixedPrecision -> FixedPrecision -> Property
propTimesDivide x y = y /= 0 ==> (x * y) / y == x

propSqrootMinErr :: FixedPrecision -> Property
propSqrootMinErr x = x >= 0 ==> rdiff == minimum diffs
  where
  (FP rm re) = sqroot x
  diffs = [abs (x - y*y) | rm' <- [rm-1 .. rm+1], 0<=rm', rm'<=9999,
                           let y = (FP rm' re) ]
  rdiff =  abs (x - y*y) where y = (FP rm  re)

testFixedPrecision d = do
  putStrLn "norm"
  depthCheck d propNorm
  putStrLn "ordReflexive"
  depthCheck d propOrdReflexive
  putStrLn "ordAntisymmetric"
  depthCheck d propOrdAntisymmetric
  putStrLn "ordTransitive"
  depthCheck (d-1) propOrdTransitive
  putStrLn "eqReflexive"
  depthCheck d propEqReflexive
  putStrLn "eqSymmetric"
  depthCheck d propEqSymmetric
  putStrLn "eqTransitive"
  depthCheck (d-1) propEqTransitive
  putStrLn "plusZero"
  depthCheck d propPlusZero
  putStrLn "plusCommutes"
  depthCheck d propPlusCommutes
  putStrLn "timesZero"
  depthCheck d propTimesZero
  putStrLn "timesOne"
  depthCheck d propTimesOne
  putStrLn "timesCommutes"
  depthCheck d propTimesCommutes
  putStrLn "doubleAdd"
  depthCheck d propDoubleAdd
  putStrLn "negatePlus"
  depthCheck d propNegatePlus
  putStrLn "negateMinus"
  depthCheck d propNegateMinus
  putStrLn "plusMinus"
  depthCheck d propPlusMinus
  putStrLn "recipDivide"
  depthCheck d propRecipDivide
  putStrLn "timesDivide"
  depthCheck d propTimesDivide
  putStrLn "sqrootSquare"
  depthCheck d propSqrootMinErr

-- Correspondence between CFP and FixedPrecision

toCFP :: FixedPrecision -> CFP
toCFP fp = sampleOf (fromIntegral $ mantissa fp) (fromIntegral $ exponent fp)

fromCFP :: CFP -> FixedPrecision
fromCFP cfp = FP (fromIntegral $ fixedMan cfp) (fromIntegral $ fixedExp cfp)

propToFrom :: FixedPrecision -> Bool
propToFrom x = fromCFP (toCFP x) == x

lift2 :: (CFP -> CFP -> CFP) -> FixedPrecision -> FixedPrecision -> FixedPrecision
lift2 op x y = fromCFP (toCFP x `op` toCFP y)

propPlus :: FixedPrecision -> FixedPrecision -> Bool
propPlus x y = (x + y) == fromCFP (toCFP x + toCFP y)

propMinus :: FixedPrecision -> FixedPrecision -> Bool
propMinus x y = (x - y) == fromCFP (toCFP x - toCFP y)

propTimes :: FixedPrecision -> FixedPrecision -> Bool
propTimes x y = (x * y) == fromCFP (toCFP x * toCFP y)

propDivide :: FixedPrecision -> FixedPrecision -> Property
propDivide x y = y /= 0 ==> (x / y) == fromCFP (toCFP x / toCFP y)

propRecip :: FixedPrecision -> Property
propRecip x = x /= 0 ==> recip x == fromCFP (fixedPrecRecip (toCFP x))

propSqroot :: FixedPrecision -> Property
propSqroot x = x >= 0 ==> sqroot x == fromCFP (fixedPrecSqroot (toCFP x))

propIntSqroot :: Int -> Property
propIntSqroot n = n >= 0 ==> intSqrootC n == intSqroot n
