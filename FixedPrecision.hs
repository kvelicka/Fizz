module FixedPrecision where

import Data.Char
import Data.Int
import Data.Ratio
import Prelude hiding (exponent)
import Text.ParserCombinators.Poly

import IntOps

data FixedPrecision = FP
    { mantissa :: Int32  	-- 4 digits, range +/- 1000-9999
    , exponent :: Int32  	-- 3 digits, range +/- 000-999
    } deriving (Eq)

instance Show FixedPrecision where
  show fp = show (mantissa fp) ++ "e" ++ show (exponent fp)

normaliseFP :: FixedPrecision -> FixedPrecision
normaliseFP fp@(FP m e) | m == 0        = FP m 0
                        | abs m < 1000  = normaliseFP $ FP (m*10)    (e-1)
                        | abs m > 9999  = normaliseFP $ FP (round m) (e+1)
                        | otherwise     = fp
  where round m = let d  = m `rem` 10
                      m' = m `quot` 10
                  in if d >= 5 then m'+1
                     else if d <= (-5) then m'-1
                     else m'

instance Ord FixedPrecision where
    compare (FP 0 _)   (FP m1 e1) = compare 0 m1
    compare (FP m0 e0) (FP 0 _)   = compare m0 0
    compare (FP m0 e0) (FP m1 e1) =
        case compare (signum m0) (signum m1) of
          EQ -> case compare e0 e1 of
                  EQ  -> compare m0 m1
                  cmp -> cmp
          cmp -> cmp

instance Num FixedPrecision where
    (FP m0 e0) * (FP m1 e1) = normaliseFP $ FP (m0*m1) (e0+e1)
    fp0@(FP 0 _)   + fp1@(FP m1 e1) = fp1
    fp0@(FP m0 e0) + fp1@(FP 0 _)   = fp0
    fp0@(FP m0 e0) + fp1@(FP m1 e1) =
        let diff = e1-e0
        in if diff > 5 then fp1
        else if diff < (-5) then fp0
        else if diff >=0 then normaliseFP $ FP (m1*(10^diff)+m0)    e0
                         else normaliseFP $ FP (m0*(10^(-diff))+m1) e1
    fp0 - fp1 = fp0 + negate fp1
    negate (FP m e) = FP (-m) e
    abs    (FP m e) = FP (abs m) e
    signum (FP m e) = normaliseFP $ FP (signum m) 0
    fromInteger i   = chop i 0
        where chop i e | i < (toInteger (maxBound::Int))
                                   = normaliseFP (FP (fromInteger i) e)
                       | otherwise = chop (i`quot`10) (e+1)

instance Real FixedPrecision where
    toRational (FP m e) | e<0       = toInteger m % (10 ^ negate (toInteger e))
                        | otherwise = toInteger m * (10^e) % 1
instance Fractional FixedPrecision where
    (FP m0 e0) / (FP m1 e1) = normaliseFP $ FP ((m0*10000)`quot`m1) (e0-e1-4)
    recip (FP m e)   = normaliseFP $ FP (10000000`quot`m) (-e-7)
    fromRational rat = error "not implemented: fromRational on FixedPrecision"


sqroot :: FixedPrecision -> FixedPrecision
sqroot (FP 0 0) = FP 0 0
sqroot (FP m e) | m < 0  = error "sqroot: negative argument"
                | odd e  = sqroot' (m*10) (e-1)
                | even e = sqroot' m e

sqroot' :: Int32 -> Int32 -> FixedPrecision
sqroot' m e = if m < 1000000 then sqroot' (m*100) (e-2)
              else normaliseFP (FP (fromIntegral (intSqroot (fromIntegral m))) (e `quot` 2))

----
digit  :: Parser Char Char
--digit   = satisfy isDigit
digit   = next	-- cheaper!

sign   :: Parser Char Bool
sign    = (do satisfy (=='-'); return True)
          `onFail`
          (do satisfy (=='+'); return False)
          `onFail`
          return False

sample :: Parser Char FixedPrecision
sample  = do neg <- sign
             m   <- digit
             '.' <- next `onFail` fail "missing . in number"
             commit $ do
             antissa <- exactly 3 digit
             'E' <- next `onFail` fail "missing E in number"
             expneg <- sign
             exponent <- exactly 3 digit
             return (FP (convert neg (m:antissa)) (convert expneg exponent - 3))
  where
    convert neg [a,b,c,d] =
            (if neg then negate else id) $
            (1000*origin a) + (100*origin b) + (10*origin c) + (origin d)
    convert neg [a,b,c] =
            (if neg then negate else id) $
            (100*origin a) + (10*origin b) + (origin c)
    origin x = fromIntegral (ord x - ord '0')

line :: Parser Char [FixedPrecision]
line = do ss <- exactly 9 (do s <- sample
                              satisfy (==' ')
                              return s )
          s <- sample
          satisfy (=='\n')
          return (s:ss)
