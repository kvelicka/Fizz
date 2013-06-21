{-# LANGUAGE ForeignFunctionInterface #-}
module CFP where

import Numeric
import Data.Word
import FixedPrecision

newtype CFP = CFP Word32
  deriving (Eq)

instance Show CFP where
  show x = show (fixedMan x) ++ "e" ++ show (fixedExp x)

foreign import ccall safe fixedPrecPlus   :: CFP -> CFP -> CFP
foreign import ccall safe fixedPrecMinus  :: CFP -> CFP -> CFP
foreign import ccall safe fixedPrecMult   :: CFP -> CFP -> CFP
foreign import ccall safe fixedPrecDivide :: CFP -> CFP -> CFP
foreign import ccall safe fixedPrecRecip  :: CFP -> CFP
foreign import ccall safe fixedPrecSqroot :: CFP -> CFP
foreign import ccall safe lessThan        :: CFP -> CFP -> Bool
foreign import ccall safe greaterThan     :: CFP -> CFP -> Bool

foreign import ccall safe sampleOf        :: Int -> Int -> CFP
foreign import ccall safe fixedMan        :: CFP -> Int
foreign import ccall safe fixedExp        :: CFP -> Int

foreign import ccall safe intSqrootC      :: Int -> Int
foreign import ccall safe rootBoundC      :: Int -> Int

instance Ord CFP where
  (<) = lessThan
  (>) = greaterThan

instance Num CFP where
  (+) = fixedPrecPlus
  (-) = fixedPrecMinus
  (*) = fixedPrecMult
  abs v = sampleOf (abs (fixedMan v)) (fixedExp v)
  signum v = sampleOf (signum (fixedMan v)) 0
  fromInteger v = let (FP m e) = fromInteger v
                  in sampleOf (fromIntegral m) (fromIntegral e)

instance Fractional CFP where
  (/)   = fixedPrecDivide
  recip = fixedPrecRecip
