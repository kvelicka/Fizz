{-# LANGUAGE ScopedTypeVariables #-}

{- Colour Palettes and lookup.

   We have implemented colour lookup based on two approaches@

   1.  A lookup table parameterised over Brewer colour palettes
       obtained from the ColorBrewer tool, 
       www.personal.psu.edu/ColorBrewer/ColorBrewer.html
   2.  An implementation of the procedural colourmap used by
       the vis5D tool, 
       www.ssec.wisc.edu/~billh/vis.html

   For individual surfaces, we also provide a palette of named
   colours from the standard X11 palette.
-}

module Colour where

import Control.Applicative
import Data.Array (listArray, (!))
import qualified Graphics.Rendering.OpenGL.GL as GL

import Maths

-- A small number of Brewer palettes have been selected.
data Palette = MReds | MBlues | MGreens | Yellows
             | Reds | Blues | Greens | Greys deriving (Show, Eq)

-- Standard X11 colour names.
data ColourName = Blue | Red | Green | Orange | White | Yellow
                  deriving Show


-- An important operation with colours is doing interpolation.
-- We support this in the viewer by making colours instances of two
-- powerful algebraic structures, general functors and applicative
-- functors.  This allows us to lift computations from values up
-- to computations over colours.


transfer_t :: (Real a) => (a,a) -> [GL.Color4 GL.GLfloat] -> a -> GL.Color4 GL.GLfloat
transfer_t (l,u) ramp 
    = let len = length ramp - 1
          arr = listArray (0,len) ramp
      in  \v -> let s :: Float = realToFrac (v - l) / realToFrac (u - l)
                    t :: Float = s * (toFloat $ length ramp - 1)
                    a = ceiling $ t
                    b = floor $ t
                in if s < 0.0 || s > 1.0 
                   then error "transfer_t: value out of range"
                   else if a == b 
                        then arr!a
                        else interp (t - toFloat b) (arr!a) (arr!b)


-- Transfer function used in Vis5D
transfer_f :: (Real a) => (a,a) -> a -> GL.Color4 GL.GLfloat
transfer_f (l,u) v = GL.Color4 (r/256.0) (g/256.0) (b/256.0) (toFloat a/256.0)
                   where
                       s = toFloat $ realToFrac (v - l) / realToFrac (u - l)
                       t = toFloat $ pCURVE * (s - 0.5*pBIAS)
                       r = toFloat $ 128.0 + 127.0 * atan( 7.0*t ) / 1.57;
                       g = toFloat $ 128.0 + 127.0 * (2 * exp(-7*t*t) - 1);
                       b = toFloat $ 128.0 + 127.0 * atan( -7.0*t ) / 1.57;
                       a = 255.0 * s **pALPHAPOW
                       pCURVE    = 1.4
                       pBIAS     = 1.0
                       pALPHAPOW = 2.0




x11_rgb :: GL.GLfloat -> ColourName -> GL.Color4 GL.GLfloat
x11_rgb a cn = case cn of
                 Blue     -> to_Color4 a (  0,   0, 255)
                 Red      -> to_Color4 a (255,   0,   0)
                 Green    -> to_Color4 a (  0, 255,   0)
                 Orange   -> to_Color4 a (255, 165,   0)
                 White    -> to_Color4 a (255, 255, 255)
                 Yellow   -> to_Color4 a (255, 255,   0)

brewer :: Palette -> GL.GLfloat -> [GL.Color4 GL.GLfloat]
brewer color alpha 
    = map (to_Color4 alpha) $ 
      case color of
                   MReds  -> [ (0,0,0)
                             , (0,0,0)
                             , (128,0,0)
                             , (160,0,0)
                             , (200,0,0)
                             , (240,0,0)
                             , (250,0,0)
                             , (255,0,0)
                             ]
                   MBlues -> [ (0,0,0)
                             , (0,0,0)
                             , (0,0,128)
                             , (0,0,160)
                             , (0,0,200)
                             , (0,0,240)
                             , (0,0,250)
                             , (0,0,255)
                             ]
                   MGreens-> [ (0,0,0)
                             , (0,0,0)
                             , (0,128,0)
                             , (0,160,0)
                             , (0,200,0)
                             , (0,240,0)
                             , (0,250,0)
                             , (0,255,0)
                             ]
                   Yellows-> [ (255,255,229)  -- yellow
                             , (255,247,188)
                             , (254,227,145)
                             , (254,196, 79)
                             , (254,153, 41)
                             , (236,112, 20)
                             , (204, 76, 2)
                             , (153, 52, 4)
                             , (102, 37, 6)
                             ]
                   Reds   -> [ (255,255,204)  -- red
                             , (255,237,160)
                             , (254,217,118)
                             , (254,178, 76)
                             , (253,141, 60)
                             , (252, 78, 42)
                             , (227, 26, 28)
                             , (189,  0, 38)
                             , (128,  0, 38)
                             ]
                   Greens -> [ (255,255,229)  -- green
                             , (247,252,185)
                             , (217,240,163)
                             , (173,221,142)
                             , (120,198,121)
                             , ( 65,171, 93)
                             , ( 35,132, 67)
                             , (  0,104, 55)
                             , (  0, 69, 41)
                             ]
                   Blues  -> [ (255,247,251)  -- blue
                             , (236,231,242)
                             , (208,209,230)
                             , (166,189,219)
                             , (116,169,207)
                             , ( 54,144,192)
                             , (  5,112,176)
                             , (  4, 90,141)
                             , (  2, 56, 88)
                             ]
                   Greys  -> [ (  0,  0,  0)  -- greyscale, increasing
                             , ( 37, 37, 37)
                             , ( 82, 82, 82)
                             , (115,115,115)
                             , (150,150,150)
                             , (189,189,189)
                             , (217,217,217)
                             , (240,240,240)
                             , (255,255,255)
                             ]

to_Color4 :: GL.GLfloat -> (Int, Int, Int) -> GL.Color4 GL.GLfloat
to_Color4 alpha (r,g,b) = GL.Color4 (toFloat r/255.0) (toFloat g/255.0) (toFloat b/255.0) alpha

