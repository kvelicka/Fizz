{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables,FlexibleContexts, TypeFamilies, BangPatterns #-}

{- The picture DSL.

   This section introduces the domain-specific language for
   constructing visualizations.  It is divided into three
   sections: colour schemes, data derivation, and then
   picture construction.
-}

module PictureDSL where

import Control.Applicative
import Control.Parallel.Strategies
import Data.List (genericLength)
import Debug.Trace (trace)
import Prelude hiding (lookup)
import qualified Data.ByteString as BS
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

evalPicture :: (Interp a, InvInterp a, Dataset d) => View d a -> HsScene
evalPicture (source :> (Surface pal level)) = 
  Group static [geometry]
  where
    field      = unsafePerformIO $ readData source
    values     = Dataset.stream field -- :: [Float]
    (dx,dy,dz) = dimensions $ shape field
    --mkGrid    :: [t] -> Stream Cell8 MyVertex t
    --mkGrid     = cubicGrid (dx, dy, dz)
    points     = cubicGrid (dx, dy, dz) $ cubicPoints field
    vcells     = cubicGrid (dx, dy, dz) values -- :: Stream Cell8 MyVertex Float
    tVal       = head $ samplingToList level
    colour     = transfer pal 1.0 1.0 1.0 1.0
    contour    = concat $ Algorithms.isosurface tVal vcells points -- :: [GL.Vertex3 GL.GLfloat]
    geometry   = surfaceGeom contour [colour]

evalPicture (source :> (Contour pal levels)) =
  Group static [geometry]
  where
    field    = unsafePerformIO $ readData source
    (dx,dy)  = dimensions2D $ shape field
    mkGrid  :: [a] -> Stream Cell4 MyVertex a
    mkGrid   = squareGrid (dx-1, dy-1)
    points   = mkGrid $ squarePoints field
    vcells   = mkGrid $ Dataset.stream field
    tVals    = fmap toFloat $ samplingToList levels
    colour   = transfer pal 1.0 1.0 (genericLength $ tVals)
    contours = map (\t -> concat $ Algorithms.isosurface t vcells points) $ tVals
    geometry = contourGeom contours (map colour [1.0 .. (genericLength $ tVals)])

evalPicture (source :> (Slice pal)) =
  Group static $ [plane rows]
  where
    field      = unsafePerformIO $ readData source
    values     = Dataset.stream field
    (dx,dy,dz) = dimensions $ shape field
    points     = planePoints dx dy dz
    colour     = transfer pal 1.0 (minimum $ values) (maximum $ values)
    colours   :: [GL.Color4 GL.GLfloat] = map colour values
    rows       = splitInto dx {- steps -} $ zip points colours
    --steps  = case slice_plane (space field) of
    --           Xequals _ -> dx
    --           Yequals _ -> dy
    --           Zequals _ -> dz

evalPicture (source :> (Volume pal)) = 
  volumeGeom (dx,dy,dz) points colours
  where
    field      = unsafePerformIO $ readData source
    values     = Dataset.stream field
    (dx,dy,dz) = dimensions $ shape field
    points     = cubicPoints field
    colour     = transfer pal 0.4 (minimum $ values) (maximum $ values)
    colours   :: [GL.Color4 GL.GLfloat] = map colour values

evalPicture (source :> Draw ps) =
  Group static $ map (\x -> evalPicture (source :> x) ) ps

evalPicture (source :> Anim ps) = 
  Animate animControl True (map (\x -> evalPicture (source :> x)) ps) []

-- Placeholder, needs rewriting
planePoints :: Int -> Int -> Int -> [GL.Vertex3 GL.GLfloat]
planePoints dx dy dz
  | dx == 1 = trace "dx evaluated" $ [GL.Vertex3 0.0 (realToFrac y) (realToFrac z)   | y <- [0 .. dy-1], z <- [0..dz-1]]
  | dy == 1 = trace "dy evaluated" $ [GL.Vertex3 (realToFrac x) 0.0 (realToFrac z)   | x <- [0 .. dx-1], z <- [0..dz-1]]
  | dz == 1 = trace "dz evaluated" $ [GL.Vertex3 (realToFrac x) (realToFrac y) 124.0 | y <- [0 .. dy-1], x <- [0..dx-1]]
