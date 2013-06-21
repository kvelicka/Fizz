{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Sample where

import Prelude hiding (exponent)
import FixedPrecision
import Foreign
import Data.Word

newtype Sample = Sample { s :: Word32 }
    deriving (Storable, Show)

toSample :: FixedPrecision -> Sample
toSample fp = Sample $
              fromIntegral (exponent fp ) `besides` fromIntegral (mantissa fp)
  where
    besides :: Word32 -> Word32 -> Word32
    besides e m = (e `shiftL` 16)  .|. (m .&. 0x0000ffff)

fromSample :: Sample -> FixedPrecision
fromSample (Sample s) = FP m e
  where
    m32 :: Int32 = fromIntegral $ s `shiftL` 16
    m = fromIntegral $ m32 `shiftR` 16
    e32 :: Int32 = fromIntegral $ s
    e = fromIntegral $ e32 `shiftR` 16

prop_sample :: FixedPrecision -> Bool
prop_sample fp = fp == (fromSample $ toSample fp)

prop_fp_float :: FixedPrecision -> Bool
prop_fp_float fp =
    let lhs :: Float = ((fromIntegral $ mantissa fp) * 10.0 ^^ (exponent fp))
    in lhs == (fromRational.toRational $ fp)

