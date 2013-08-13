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

import Data.Array (listArray, (!))
import qualified Graphics.Rendering.OpenGL.GL as GL

import Maths

-- A small number of Brewer palettes have been selected.
data Palette = MReds | MBlues | MGreens | Yellows
             | Reds | Blues | Greens | Greys deriving (Show, Eq)

-- Standard X11 colour names.
data ColourName = Blue | Red | Green | Orange | White | Yellow
                  deriving Show

type Lookup a b = [(a, b)]                  

-- An important operation with colours is doing interpolation.
-- We support this in the viewer by making colours instances of two
-- powerful algebraic structures, general functors and applicative
-- functors.  This allows us to lift computations from values up
-- to computations over colours.

-- Old functions that are slow but a replacement has not been written yet
transfer :: Colour -> Float -> Float -> Float -> Float -> GL.Color4 GL.GLfloat
transfer Vis5D _     minv maxv 
    = transferF (minv, maxv)
transfer (Brewer color) alpha minv maxv 
    = lookupTab $ buildTable (minv, maxv) (brewer color (realToFrac alpha) )
transfer (X11 names) alpha minv maxv
    = lookupTab $ buildTable (minv, maxv) (map (x11Rgb (realToFrac alpha)) names)

lookupTab :: (Real a, InvInterp a, Interp b) => Lookup a b -> a -> b
lookupTab p v 
    = case lookup' Nothing p v of
        (Nothing,      Just c)       -> snd c
        (Just c,       Nothing)      -> snd c
        (Just (k1,c1), Just (k2,c2)) -> interp (invInterp v k1 k2) c1 c2
      where
        lookup' prev (e:ps) v | v <= fst e = (prev,   Just e)
                              | null ps    = (Just e, Nothing)
                              | otherwise  = lookup' (Just e) ps v

buildTable :: (Enum a, Fractional a) => (a,a) -> [b] -> Lookup a b
buildTable (l,u) cols = zip [l, l+step ..] cols
                         where step = (u - l) / realToFrac (length cols - 1)


transferT :: (Real a) => (a,a) -> [GL.Color4 GL.GLfloat] -> a -> GL.Color4 GL.GLfloat
transferT (l,u) ramp 
    = let len = length ramp - 1
          arr = listArray (0,len) ramp
      in  \v -> let s :: Float = realToFrac (v - l) / realToFrac (u - l)
                    t :: Float = s * (toFloat $ length ramp - 1)
                    a = ceiling $ t
                    b = floor $ t
                in if s < 0.0 || s > 1.0 
                   then error "transferT: value out of range"
                   else if a == b 
                        then arr!a
                        else interp (t - toFloat b) (arr!a) (arr!b)

-- Transfer function used in Vis5D
transferF :: (Real a) => (a,a) -> a -> GL.Color4 GL.GLfloat
transferF (l,u) v = GL.Color4 (r/256.0) (g/256.0) (b/256.0) (toFloat a/256.0)
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

x11Rgb :: GL.GLfloat -> ColourName -> GL.Color4 GL.GLfloat
x11Rgb a cn = case cn of
                 Blue     -> toColor4 a (  0,   0, 255)
                 Red      -> toColor4 a (255,   0,   0)
                 Green    -> toColor4 a (  0, 255,   0)
                 Orange   -> toColor4 a (255, 165,   0)
                 White    -> toColor4 a (255, 255, 255)
                 Yellow   -> toColor4 a (255, 255,   0)

brewer :: Palette -> GL.GLfloat -> [GL.Color4 GL.GLfloat]
brewer color alpha 
    = map (toColor4 alpha) $ 
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

toColor4 :: GL.GLfloat -> (Int, Int, Int) -> GL.Color4 GL.GLfloat
toColor4 alpha (r,g,b) = GL.Color4 (toFloat r/255.0) (toFloat g/255.0) (toFloat b/255.0) alpha

-- Colours for the picture DSL ----------------------------------------
--
-- We are typically interested in palettes of multiple colours,
-- and provide various methods for constructing these.  For
-- cases where we only want one colour, we also provide an
-- easy way of generating the appropriate (singleton) palette.
--
-- A colour scheme is either a brewer palette, the 
-- procedural colour generator from Vis5D, or a single
-- X11 colour specified by a (standard) name.

data Colour = Brewer Palette | Vis5D | X11 [ColourName] deriving Show

-- For ease of specification, we define some simple "smart
-- constructors" for building colour schemes, applying the
-- suitable wrapper.  Note that the constructor function is 
-- simply the lower case version of the data constructor.

reds, blues, greens, greys :: Colour
mreds   = Brewer MReds
mblues  = Brewer MBlues
mgreens = Brewer MGreens
reds    = Brewer Reds
blues   = Brewer Blues
greens  = Brewer Greens
greys   = Brewer Greys
yellows = Brewer Yellows

vis5D :: Colour
vis5D  = Vis5D

blue, red, green, orange, white, yellow :: Colour
blue    = X11 [Blue]
red     = X11 [Red]
green   = X11 [Green]
orange  = X11 [Orange]
white   = X11 [White]
yellow  = X11 [Yellow]
