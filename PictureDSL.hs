{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables,FlexibleContexts, TypeFamilies #-}

{- The picture DSL.

   This section introduces the domain-specific language for
   constructing visualizations.  It is divided into three
   sections: colour schemes, data derivation, and then
   picture construction.
-}

module PictureDSL where

import Control.Applicative
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
            -- | Volume Colour
            -- | Contour Colour (Sampling v) 
            --   Compound expressions also disabled for now
            -- | Draw [Picture v]
            -- | Anim [Picture v]

-- View datatype combines a source with a picture description to make a generic
-- picture type that is independent of the source
data View d v = d :> (Picture v)

evalPicture :: (Enum a, Interp a, InvInterp a, Dataset d) => View d a -> HsScene
evalPicture (source :> (Surface pal levels)) = 
  unsafePerformIO(putStrLn $ show dx ++ " " ++ show dy ++ " " ++ show dz) `seq`
  Group static geomlist
  where
    field      = unsafePerformIO $ readData source
    (dx,dy,dz) = dimensions $ shape field
    mkGrid    :: [a] -> Stream Cell_8 MyVertex a
    mkGrid     = cubicGrid (dx,dy,dz)
    points     = mkGrid $ cubicPoints field
    vcells     = mkGrid $ Dataset.stream field
    t_vals     = fmap toFloat $ samplingToList levels
    colour     = transfer pal 1.0 1.0 (toFloat.length $ t_vals)
    contours   = map (\t -> concat $ Algorithms.isosurface t vcells points) $ t_vals
    geomlist   = zipWith surface_geom contours $ repeat (map colour [1.0 .. (toFloat.length $ t_vals)])

evalPicture (source :> (Slice pal)) =
  Group static $ [plane rows]
  where
      field      = unsafePerformIO $ readData source
      values     = Dataset.stream field
      (dx,dy,dz) = dimensions $ shape $ field
      points     = planePoints dx dy dz
      colour     = transfer pal 1.0 (minimum $ values) (maximum $ values)
      colours   :: [GL.Color4 GL.GLfloat] = map colour values
      rows       = splitInto dx {- steps -} $ zip points colours
      --steps  = case slice_plane (space field) of
      --           X_equals _ -> dx
      --           Y_equals _ -> dy
      --           Z_equals _ -> dz

planePoints :: Int -> Int -> Int -> [GL.Vertex3 GL.GLfloat]
planePoints dx dy dz
  | dx == 1 = trace "dx evaluated" $ [GL.Vertex3 0.0 (realToFrac y) (realToFrac z)   | y <- [0 .. dy-1], z <- [0..dz-1]]
  | dy == 1 = trace "dy evaluated" $ [GL.Vertex3 (realToFrac x) 0.0 (realToFrac z)   | x <- [0 .. dx-1], z <- [0..dz-1]]
  | dz == 1 = trace "dz evaluated" $ [GL.Vertex3 (realToFrac x) (realToFrac y) 124.0 | y <- [0 .. dy-1], x <- [0..dx-1]]

{-
eval_picture env (Volume pal de)
    = volume_geom dim points colours
      where
          field  = env -- eval_data env de
          dim    = (dim_x env, dim_y env, dim_z env) -- field
          points = cubicPoints field
          colour = transfer pal 0.4 (minv field) (maxv field)
          colours :: [GL.Color4 GL.GLfloat] = map colour (values field)
-}
{- 
eval_picture (Contour pal levels field)
    = Group static [geomlist]
      where
          -- (Use ads) = de -- eval_data env de
          -- field = read_astro ads
          mkgrid = cubicGrid (shape field)
          points = mkgrid $ cubicPoints field
          vcells = mkgrid $ field
          t_vals = range_to_list levels
          colour = transfer pal 1.0 1.0 (toFloat.length $ t_vals)
          --contours = map (\t -> concat $ Algorithms.iso t vcells points) $ t_vals
          --arr = values field
          --surf t = concat $ isosurf t arr
          --contours = map surf t_vals
          contours = map (\t -> concat $ Algorithms.isosurface t (Stream . toList . values $ field)) $ t_vals
          geomlist = contour_geom contours (map colour [1.0 .. (toFloat.length $ t_vals)])
-}
{-
eval_picture env (Contour pal levels de)
    = Group static [geometry]
      where
          field  = env --eval_data env de
          plane  = slice_plane (space field)
          mkgrid = squareGrid (cell_size_2D field plane)
          points = mkgrid $ plane_points (space field)
          vcells = mkgrid $ values field
          t_vals = range_to_list levels
          colour = transfer pal 1.0 1.0 (toFloat.length $ t_vals)
          contours = map (\t -> concat $ Algorithms.isosurface t vcells points) $ t_vals
          geometry = contour_geom contours (map colour [1.0 .. (toFloat.length $ t_vals)])
-}
{-
eval_picture env (Draw ps)
    = Group static $ map (eval_picture env) ps
-}
{-
eval_picture (Anim ps)
    = Animate anim_control (HsMovie True (map eval_picture ps) [])
-}
