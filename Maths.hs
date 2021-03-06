{-# LANGUAGE FlexibleInstances #-}

module Maths where

import Control.Applicative
import Data.Word
import Graphics.Rendering.OpenGL as GL

-- Interpolation -------------------------------------------------
--
-- Linear interpolation, and inverse linear interpolation, is a fundamental
-- operation, which is used for (scalar) values, coordinates, colours, and
-- other data.  We define classes to capture these two operations, and here
-- define basic instances (on floating point values), and a more general
-- version of interpolation for any applicative structure.

toFloat :: (Real a, Fractional b) => a -> b
toFloat = realToFrac

class Interp b where
  interp :: Float -> b -> b -> b

class Real b => InvInterp b where
  invInterp :: b -> b -> b -> Float

instance Interp Float where
  interp t v1 v2 = (1-t)*v1 + t*v2

instance Interp Double where
  interp t v1 v2 = let t' = realToFrac t in (1-t')*v1 + t'*v2

instance Interp Word8 where
  interp t v1 v2 = toEnum.fromEnum.round $ (1-t)*(toFloat v1) + t*(toFloat v2)

instance InvInterp Float where
  invInterp s v1 v2 = (s - v1) / (v2 - v1)

instance InvInterp Double where
  invInterp s v1 v2 = realToFrac $ (s - v1) / (v2 - v1)

instance InvInterp Word8 where
  invInterp s v1 v2 = toFloat (s - v1) / (toFloat (v2 - v1))

instance (Interp a, Applicative f) => Interp (f a) where
  interp t x y = pure (interp t) <*> x <*> y
