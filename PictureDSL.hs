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
            -- | ASurface Colour (Sampling v) 
            --  Following take 3 data expressions, need to confirm how to convert
            -- | Hedgehog Colour (DataExpr v) (DataExpr v) (DataExpr v)
            -- | Scatter (DataExpr v) (DataExpr v) (DataExpr v)
            --   Compound expressions also disabled for now
            -- | Draw [Picture v]
            -- | Anim [Picture v]

-- View datatype combines a source with a picture description to make a generic
-- picture type that is independent of the source
data View d v = d :> (Picture v)

type Lookup a b = [(a, b)]


-- Old functions that are slow but a replacement has not been written yet
transfer :: Colour -> Float -> Float -> Float -> Float -> GL.Color4 GL.GLfloat
transfer Vis5D _     minv maxv 
    = transfer_f (minv, maxv)
transfer (Brewer color) alpha minv maxv 
    = lookup_tab $ build_table (minv, maxv) (brewer color (realToFrac alpha) )
transfer (X11 names) alpha minv maxv
    = lookup_tab $ build_table (minv, maxv) (map (x11_rgb (realToFrac alpha)) names)

lookup_tab :: (Real a, InvInterp a, Interp b) => Lookup a b -> a -> b
lookup_tab p v 
    = case lookup' Nothing p v of
        (Nothing,      Just c)       -> snd c
        (Just c,       Nothing)      -> snd c
        (Just (k1,c1), Just (k2,c2)) -> interp (inv_interp v k1 k2) c1 c2
      where
        lookup' prev (e:ps) v | v <= fst e = (prev,   Just e)
                              | null ps    = (Just e, Nothing)
                              | otherwise  = lookup' (Just e) ps v

build_table :: (Enum a, Fractional a) => (a,a) -> [b] -> Lookup a b
build_table (l,u) cols = zip [l, l+step ..] cols
                         where step = (u - l) / realToFrac (length cols - 1)

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
eval_picture env (Hedgehog pal deu dev dew)
    = Geometry static GL.Lines [HsGeom_cv col verts]
      where
          fieldu = eval_data env deu
          fieldv = eval_data env dev
          fieldw = eval_data env dew
          dim    = dimensions fieldu
          points = cubicPoints fieldu
          geom   = zipWith4 hogs points (values fieldu) (values fieldv) (values fieldw)
-}
{- hedgehog (first derivative, across the cell)
hog' (rngx,rngy,rngz) cell
    = GL.Vertex3 (4* dx/rngx) (4* dy/rngy) (4* dz/rngz)
      where
          dx = sum_samples uf [Cell.B, Cell.C, Cell.F, Cell.G] - sum_samples uf [Cell.A, Cell.D, Cell.E, Cell.H]
          dy = sum_samples vf [Cell.C, Cell.D, Cell.G, Cell.H] - sum_samples vf [Cell.A, Cell.B, Cell.E, Cell.F]
          dz = sum_samples wf [Cell.E, Cell.F, Cell.G, Cell.H] - sum_samples wf [Cell.A, Cell.B, Cell.C, Cell.D]
          sum_samples cf ixs = sum $ map (cf.((flip select) cell)) $ ixs
          uf = \(u,_,_) -> u
          vf = \(_,v,_) -> v
          wf = \(_,_,w) -> w

interpret v5d ("+v" : unm : vnm : wnm : s_ti : vals)
    = (hog_geom geo $ colour vals) : interpret v5d (drop 4 vals)
      where
         geo  = hedge hog' (rng_u, rng_v, rng_w)
                           ( {-slice (Just 20,Just 20,Nothing)-} du
                           , {-slice (Just 20,Just 20,Nothing)-} dv
                           , {-slice (Just 20,Just 20,Nothing)-} dw)
         (du, gu) = dataset (fst v5d) unm ti
         (dv, gv) = dataset (fst v5d) vnm ti
         (dw, gw) = dataset (fst v5d) wnm ti

         rng_u = range (minval gu) (maxval gu)
         rng_v = range (minval gv) (maxval gv)
         rng_w = range (minval gw) (maxval gw) 
-}
{-
eval_picture (ASurface pal levels field)
    = Group static geomlist
      where
          -- (Use ads) = de -- eval_data env de
          -- field = read_astro ads
          -- mkgrid = cubicGrid (cell_size_3D field)
          -- points = mkgrid $ cubicPoints field
          -- vcells = mkgrid $ values field
          t_vals = range_to_list levels
          colour = transfer pal 1.0 1.0 (toFloat.length $ t_vals)
          --contours = map (\t -> concat $ Algorithms.iso t vcells points) $ t_vals
          --arr = values field
          --surf t = concat $ isosurf t arr
          --contours = map surf t_vals
          contours = map (\t -> Algorithms.isosurface t (toList $ values field) {- (values field) -}) $ t_vals
          geomlist = zipWith surface_geom contours $ repeat (map colour [1.0 .. (toFloat.length $ t_vals)])
-}
{-
eval_picture (AContour pal levels field)
    = Group static [geomlist]
      where
          -- (Use ads) = de -- eval_data env de
          -- field = read_astro ads
          -- mkgrid = cubicGrid (cell_size_3D field)
          -- points = mkgrid $ cubicPoints field
          -- vcells = mkgrid $ values field
          t_vals = range_to_list levels
          colour = transfer pal 1.0 1.0 (toFloat.length $ t_vals)
          --contours = map (\t -> concat $ Algorithms.iso t vcells points) $ t_vals
          --arr = values field
          --surf t = concat $ isosurf t arr
          --contours = map surf t_vals
          contours = map (\t -> concat $ Algorithms.isosurface t (values field)) $ t_vals
          geomlist = contour_geom contours (map colour [1.0 .. (toFloat.length $ t_vals)])
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
eval_picture env (Scatter ds1 ds2 ds3)
    = scatterplot str1 str2 str3
      where
          str1 = rescale . eval_data env $ ds1
          str2 = rescale . eval_data env $ ds2
          str3 = rescale . eval_data env $ ds3
          rescale field  = map (realToFrac . (scale lo hi)) $ values field
                           where
                             lo = minv field
                             hi = maxv field
                             scale :: InvInterp a => a -> a-> a -> Float
                             scale l h v = interp (inv_interp v l h) 0 248
-}
{-
eval_picture (Anim ps)
    = Animate anim_control (HsMovie True (map eval_picture ps) [])
-}
