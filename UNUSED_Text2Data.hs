import Text.ParserCombinators.Poly.Lazy
import System
import Foreign
import Data.Word
import System.IO

import Sample
import FixedPrecision

main = do
    [fin, fout] <- getArgs
    text <- readFile fin
    out  <- openBinaryFile fout WriteMode
    buf  <- mallocBytes 4
    let vals = fst $ runParser (many (sample `discard` next)) text
    mapM_ (write out buf) vals
    hFlush out
    hClose out

write :: Handle -> Ptr Word32 -> FixedPrecision -> IO ()
write h buf fp = do
    poke buf (s (toSample fp))
    hPutBuf h buf 4


module Parser where

import Text.ParserCombinators.Poly.Lazy
import Data.Int
import Data.Ratio
import Data.Char
import Prelude hiding (exponent)

data FixedPrecision = FP
    { mantissa :: Int  	-- 4 digits, range +/- 1000-9999
    , exponent :: Int  	-- 3 digits, range +/- 000-999
    } deriving (Eq)

instance Show FixedPrecision where
  show fp = show (mantissa fp) ++ "e" ++ show (exponent fp)

normaliseFP :: FixedPrecision -> FixedPrecision
normaliseFP fp@(FP m e) | m == 0        = FP m 0
                        | abs m < 1000  = normaliseFP $ FP (m*10)     (e-1)
                        | abs m > 9999  = normaliseFP $ FP (m`div`10) (e+1)
                        | otherwise     = fp

instance Ord FixedPrecision where
    compare (FP m0 e0) (FP m1 e1) =
        case compare e0 e1 of
          EQ  -> compare m0 m1
          cmp -> cmp
instance Num FixedPrecision where
    (FP m0 e0) * (FP m1 e1) = normaliseFP $ FP (m0*m1) (e0+e1)
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
                       | otherwise = chop (i`div`10) (e+1)
instance Real FixedPrecision where
    toRational (FP m e) | e<0       = toInteger m % (10 * negate (toInteger e))
                        | otherwise = toInteger m * (10^e) % 1
instance Fractional FixedPrecision where
    (FP m0 e0) / (FP m1 e1) = normaliseFP $ FP ((m0*1000)`div`m1) (e0-e1-3)
    recip (FP m e)   = normaliseFP $ FP (10000000`div`m) (-e-7)
    fromRational rat = error "not implemented: fromRational on FixedPrecision"
             

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
    origin x = ord x - ord '0'

line :: Parser Char [FixedPrecision]
line = do ss <- exactly 9 (do s <- sample
                              satisfy (==' ')
                              return s )
          s <- sample
          satisfy (=='\n')
          return (s:ss)
