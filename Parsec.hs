module Parsec where

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import Data.Int

import qualified FixedPrecision as FP

data Sign = Minus | Plus

(<|>) = (P.<|>)

parseNumbers :: P.Parser [FP.FixedPrecision]
parseNumbers = parseNumber `P.sepBy` (P.space <|> P.char ',')

parseNumber :: P.Parser FP.FixedPrecision
parseNumber = do
  mantissa <- parseMantissa
  P.char 'E' <|> fail "No 'E'"
  exponent <- parseExponent
  return $ FP.FP mantissa exponent

parseMantissa :: P.Parser Int32
parseMantissa = do
  sign <- parseSign
  firstDigit <- P.count 1 P.digit
  P.char '.' <|> fail "No point"
  restDigits <- P.count 3 P.digit
  return $ convert sign $ firstDigit ++ restDigits

parseExponent :: P.Parser Int32
parseExponent = do
  sign <- parseSign
  digits <- P.count 3 P.digit
  return $ convert sign digits

parseSign :: P.Parser Sign
parseSign =
  P.option Plus $ do
    s <- P.char '-' <|> P.char '+' <|> fail "No sign"
    return $ case s of
      '-' -> Minus
      '+' -> Plus

convert :: Sign -> String -> Int32
convert sign digits =
  case sign of
    Minus -> negate i
    Plus -> i
    where i = read digits