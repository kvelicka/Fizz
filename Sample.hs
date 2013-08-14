{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Sample where

import Foreign
import Prelude hiding (exponent)
import qualified Data.ByteString as BS

import FixedPrecision

newtype Sample = Sample { s :: Word32 }
    deriving (Storable, Show)


-- UNUSED
{-
toSample :: FixedPrecision -> Sample
toSample fp = Sample $
              fromIntegral (exponent fp ) `besides` fromIntegral (mantissa fp)
  where
    besides :: Word32 -> Word32 -> Word32
    besides e m = (e `shiftL` 16)  .|. (m .&. 0x0000ffff)
-}

fromSample :: Sample -> FixedPrecision
fromSample (Sample s) = FP m e
  where
    m32 :: Int32 = fromIntegral $ s `shiftL` 16
    m = fromIntegral $ m32 `shiftR` 16
    e32 :: Int32 = fromIntegral $ s
    e = fromIntegral $ e32 `shiftR` 16

bytesToSamples :: BS.ByteString -> [Sample]
bytesToSamples bs 
    | BS.null bs   = []
    | otherwise      = (Sample s):bytesToSamples post
                       where
                           -- INEFFICIENT
                           (pre,post) = BS.splitAt 4 bs
                           [a,b,c,d]  = BS.unpack pre
                           s = (a32 .|. b32 .|. c32 .|. d32)
                           a32 :: Word32 = fromIntegral a `shiftL` 24
                           b32 :: Word32 = fromIntegral b `shiftL` 16
                           c32 :: Word32 = fromIntegral c `shiftL`  8
                           d32 :: Word32 = fromIntegral d

sampleToFloat :: Sample -> Float
sampleToFloat = fromRational . toRational . fromSample

bytesToFloats :: BS.ByteString -> [Float]
bytesToFloats bs = map sampleToFloat $ bytesToSamples bs

{-
propSample :: FixedPrecision -> Bool
propSample fp = fp == (fromSample $ toSample fp)
-}
propFpFloat :: FixedPrecision -> Bool
propFpFloat fp =
    let lhs :: Float = ((fromIntegral $ mantissa fp) * 10.0 ^^ (exponent fp))
    in lhs == (fromRational.toRational $ fp)
