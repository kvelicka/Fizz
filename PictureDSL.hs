{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables,FlexibleContexts, TypeFamilies, BangPatterns #-}

{- The picture DSL.

   This section introduces the domain-specific language for
   constructing visualizations.  It is divided into three
   sections: colour schemes, data derivation, and then
   picture construction.
-}

module PictureDSL where

import Data.List (genericLength)
import qualified Graphics.Rendering.OpenGL.GL as GL
import System.IO.Unsafe

import Algorithms
import AstroData
import CellTypes
import Colour
import Dataset
import Graphics
import Maths
import RectGrid
import Render

fromFull :: Time -> Species -> VisData
fromFull t s = astroFull t s

from4 :: Time -> Species -> VisData
from4 t s = astroFour t s

-- 2. Pictures ------------------------------------------------------
--
-- The Picture type gives the top-level graphics that
-- can be generated.  These are 2D contour plots, 
-- 3D isosurface extraction (we allow for > 1 surface
-- to be pulled at a time), 3D volume rendering,
-- a 2D volumetric slice (smooth-shaded), or a 3D
-- scatter plot.  For the Contour command, the slice
-- plane used is implicit - one of the dataset dimensions
-- must resolve to a singleton value, and this is the
-- value that is used.
--
-- Two kinds of compound picture can be constructed:
-- pictures composed within a single frame, or 
-- pictures composed over time (an animation).
--
-- In the case of an animation, the following key 
-- controls are provided via the user interface:
--   s - stop the animation
--   g - start the animation running
--   < - back one frame
--   > - forward one frame

data Picture v =  Surface Colour (Sampling v) 
                | Slice  Colour
                | Volume Colour
                | Contour Colour (Sampling v) 
                | Draw [Picture v]
                | Anim [Picture v]

-- View datatype combines a source with a picture description to make a generic
-- picture type that is independent of the source
data View d v = d :> (Picture v)

evalPicture :: (Enum a, Interp a, InvInterp a, Dataset d) => View d a -> HsScene
evalPicture (source :> (Surface pal level)) = 
  Group static [geometry]
  where
    field      = unsafePerformIO $ readData source
    values     = datastream field
    (dx,dy,dz) = dimensions $ shape field
    mkGrid    :: [t] -> Stream Cell8 MyVertex t
    mkGrid     = cubicGrid (dx, dy, dz)
    points     = mkGrid $ cubicPoints field
    vcells     = mkGrid $ values 
    tVal       = toFloat $ head $ samplingToList level
    colour     = transfer pal 1.0 1.0 1.0 1.0
    contour    = concat $ isosurface tVal vcells points
    geometry   = surfaceGeom contour [colour]

evalPicture (source :> (Contour pal levels)) =
  Group static [geometry]
  where
    field    = unsafePerformIO $ readData source
    (dx,dy)  = dimensions2D $ shape field
    mkGrid  :: [a] -> Stream Cell4 MyVertex a
    mkGrid   = squareGrid (dx, dy)
    points   = mkGrid $ squarePoints field
    vcells   = mkGrid $ datastream field
    tVals    = fmap toFloat $ samplingToList levels
    colour   = transfer pal 1.0 1.0 (genericLength $ tVals)
    contours = map (\t -> concat $ isosurface t vcells points) $ tVals
    geometry = contourGeom contours (map colour [1.0 .. (genericLength $ tVals)])

evalPicture (source :> (Slice pal)) =
  Group static $ [plane rows]
  where
    field      = unsafePerformIO $ readData source
    values     = datastream field
    (dx,dy,dz) = dimensions $ shape field
    points     = planePoints $ shape field
    colour     = transfer pal 1.0 (minimum $ values) (maximum $ values)
    colours   :: [GL.Color4 GL.GLfloat] = map colour values
    rows       = splitInto steps $ zip points colours
    steps  = case slicePlane (shape field) of
               X_equals _ -> dy
               Y_equals _ -> dx
               Z_equals _ -> dx

evalPicture (source :> (Volume pal)) = 
  volumeGeom (dx,dy,dz) points colours
  where
    field      = unsafePerformIO $ readData source
    values     = datastream field
    (dx,dy,dz) = dimensions $ shape field
    points     = cubicPoints field
    colour     = transfer pal 0.4 (minimum $ values) (maximum $ values)
    colours   :: [GL.Color4 GL.GLfloat] = map colour values

evalPicture (source :> Draw ps) =
  Group static $ map (\x -> evalPicture (source :> x) ) ps

evalPicture (source :> Anim ps) = 
  Animate animControl True (map (\x -> evalPicture (source :> x)) ps) []
