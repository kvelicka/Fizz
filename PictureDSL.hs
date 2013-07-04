{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables,FlexibleContexts, TypeFamilies #-}

{- The picture DSL.

   This section introduces the domain-specific language for
   constructing visualizations.  It is divided into three
   sections: colour schemes, data derivation, and then
   picture construction.
-}

module PictureDSL where

--import Graphics.Rendering.OpenGL.GL.BasicTypes
--import Graphics.Rendering.OpenGL.GL.VertexSpec
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


-- Implementing the DSL --------------------------------------
--
-- Calculate the astro files required to generate a given picture.

--file_list :: (Real v, Dataset Source) => View v -> [IO (FizzData3D v)]
-- file_list (Contour _ _ d)    = expr_list d
--file_list (source :> _) = [readData source]
-- file_list (Volume _ d)       = expr_list d
-- file_list (Slice _ d)        = expr_list d
-- file_list (Scatter d1 d2 d3) = concatMap expr_list [d1, d2, d3]
-- file_list (Draw ps)          = concatMap file_list ps
-- file_list (Anim ps)          = concatMap file_list ps

--expr_list :: DataExpr v -> [(String, IO (Grid sh v))]
--expr_list (Use d)      = [(resource d, read_data d)]
--expr_list (Derive _ e) = expr_list e
--expr_list (Select _ e) = expr_list e

-- For the Contour command, the slice plane is implicit;
-- the following function determines which plane is 
-- being used.

{-
slice_plane :: Dimension -> Plane
slice_plane (Dim3D r _ _) | singleton r = X_equals . head . range_to_list $ r
slice_plane (Dim3D _ r _) | singleton r = Y_equals . head . range_to_list $ r
slice_plane (Dim3D _ _ r) | singleton r = Z_equals . head . range_to_list $ r
slice_plane _                           = error "slice_plane: no singleton dimension"

singleton :: Ord a => Sampling a -> Bool 
singleton (Single _)      = True
singleton (Range f t)     = f == t
singleton (Sampled f s t) = f == t || f == s || s > t

plane_points :: Dimension -> [GL.Vertex3 GL.GLfloat]
plane_points dim
    = case slice_plane dim of
        X_equals v -> [coord v cy cz | cz <- rng_z, cy <- rng_y]
        Y_equals v -> [coord cx v cz | cz <- rng_z, cx <- rng_x]
        Z_equals v -> [coord cx cy v | cy <- rng_y, cx <- rng_x]
      where
        rng_z = range_to_list . dim3d_z $ dim
        rng_y = range_to_list . dim3d_y $ dim
        rng_x = range_to_list . dim3d_x $ dim
        coord x y z = GL.Vertex3 (toFloat x) (toFloat y) (toFloat z)
-}

{-
cell_size_2D :: Grid a -> Plane -> (Int, Int)
cell_size_2D f (X_equals _) = (dim_y f - 1, dim_z f - 1)
cell_size_2D f (Y_equals _) = (dim_x f - 1, dim_z f - 1)
cell_size_2D f (Z_equals _) = (dim_x f - 1, dim_y f - 1)

cell_size_3D :: Grid a -> (Int, Int, Int)
cell_size_3D f = (dim_x f - 1, dim_y f - 1, dim_z f - 1)

-}

transfer :: Real v => Colour -> GL.GLfloat -> v -> v -> v -> GL.Color4 GL.GLfloat
transfer Vis5D _     minv maxv 
    = transfer_f (minv, maxv)
transfer (Brewer color) alpha minv maxv 
    = transfer_t (minv, maxv) (brewer color alpha)
transfer (X11 names) alpha minv maxv
    = transfer_t (minv, maxv) (map (x11_rgb alpha) names)


-- DSL evaluation --------------------------------------------------
--
-- Step 1 - interpretation of data expressions.  These will 
-- yield a field (see RectGrid) that contains the values to
-- be visualized.


--eval_data :: (Eq v) => Context -> Source d -> FizzData3D v
--eval_data env d      = lookup env (show $ readData d)

{-
eval_data env (Derive f de) = ds { values = (map f) . values $ ds 
                                 , minv   = f $ minv ds
                                 , maxv   = f $ maxv ds
                                 }
                              where
                                  ds = eval_data env de
eval_data env (Select p de) = sample_plane p (eval_data env de)
-}

{-
sample_plane :: Eq a => Plane -> Grid a -> Grid a
sample_plane (X_equals v) f | v `elem` (range_x f)
    = f { space  = (space f) { dim3d_x = Single v }
        , values = filterp (((toFloat v)==) . x_) f
        }
sample_plane (Y_equals v) f | v `elem` (range_y f)
    = f { space  = (space f) { dim3d_y = Single v }
        , values = filterp (((toFloat v)==) . y_) f
        }
sample_plane (Z_equals v) f | v `elem` (range_z f)
    = f { space  = (space f) { dim3d_z = Single v }
        , values = filterp (((toFloat v)==) . z_) f
        }
sample_plane p _ 
    = error "Attempting to sample on a plane not present in dataset."

filterp :: (GL.Vertex3 GL.GLfloat -> Bool) -> Grid a -> [a]
filterp p f = map fst . filter (p.snd) $ zip (values f) (cubicPoints f)
-}


-- Step 2: Picture evaluation
--
-- Essentially a much neater implementation of the various
-- details previously resolved through the command-line
-- interpreter.

{-
isosurf :: (Cell (ICell sh) a, Applicative (IVert sh), InvInterp a, IsoCells sh, Elt a,
            (IVert sh)  ~ GL.Vertex3 ) =>
           a -> A.Array sh a -> [[IVert sh Float]]
isosurf = Algorithms.iso           
-}

evalPicture :: (Enum a, Interp a, InvInterp a, Dataset d) => View d a -> HsScene
evalPicture (source :> (Surface pal levels)) = 
  Group static geomlist
  where
    field = unsafePerformIO $ readData source
    mkGrid :: [a] -> Stream Cell_8 MyVertex a
    mkGrid = cubicGrid (shape field)   
    points = trace "points" $ mkGrid $ cubicPoints field
    vcells = trace "vcells" $ mkGrid $ Dataset.stream field
    t_vals = trace "t_vals" $ fmap toFloat $ samplingToList levels
    colour = transfer pal 1.0 1.0 (toFloat.length $ t_vals)
    contours = trace "contours" $ map (\t -> concat $ Algorithms.isosurface t vcells points) $ t_vals
    geomlist = trace "geomlist" $ zipWith surface_geom contours $ repeat (map colour [1.0 .. (toFloat.length $ t_vals)])

evalPicture (source :> (Slice pal)) =
  Group static $ [plane rows]
  where
      field  = unsafePerformIO $ readData source
      (dz,dy,dx)    = dimensions field
      points = trace (show dx ++ " " ++ show dy ++ " " ++ show dz) $ plane_points dx dy dz
      colour = transfer pal 1.0 (minimum $ Dataset.stream $ field) (maximum $ Dataset.stream $ field)
      colours :: [GL.Color4 GL.GLfloat] = map colour (Dataset.stream field)
      --steps  = case slice_plane (space field) of
      --           X_equals _ -> dx
      --           Y_equals _ -> dy
      --          Z_equals _ -> dz
      rows   = splitInto dz {- steps -} $ zip points colours

plane_points :: Int -> Int -> Int -> [GL.Vertex3 GL.GLfloat]
plane_points dx dy dz
  | dx == 1   =   trace "dx evaluated" $ [GL.Vertex3 0.0 (realToFrac y) (realToFrac z) | y <- [0 .. dy-1], z <- [0..dz-1]]
  | dy == 1   =   trace "dy evaluated" $ [GL.Vertex3 (realToFrac x) 0.0 (realToFrac z) | x <- [0 .. dy-1], z <- [0..dz-1]]
  | dz == 1   =   trace "dz evaluated" $ [GL.Vertex3 (realToFrac x) (realToFrac y) 0.0 | x <- [0 .. dy-1], y <- [0..dz-1]]


--evalPicture (source :> (Draw ps)) =
-- Group static $ map evalPicture ps

{- Reason: Old picture DSL evaluator, replaced with evalPicture
eval_picture :: (Enum a, Interp a, InvInterp a) => Context -> Picture a -> HsScene
                
eval_picture env (Surface pal levels field)
    = Group static geomlist
      where
          (Use ads) = eval_data env field
          field = read_astro_data ads
          points = mkgrid $ cubicPoints field       -- :: Stream Cell_8 MyVertex Vertex3
          vcells = mkgrid $ extract $ values field  -- :: Stream Cell_8 MyVertex Float
          t_vals = fmap toFloat $ range_to_list levels             -- :: Num a ??
          colour = transfer pal 1.0 1.0 (toFloat.length $ t_vals)
          contours = map (\t -> concat $ Algorithms.isosurface t vcells points) $ t_vals
          geomlist = zipWith surface_geom contours $ repeat (map colour [1.0 .. (toFloat.length $ t_vals)])

eval_picture (Draw ps)
    = Group static $ map eval_picture ps
-}
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
slice_plane :: Dimension -> Plane
slice_plane (Dim3D r _ _) | singleton r = X_equals . head . range_to_list $ r
slice_plane (Dim3D _ r _) | singleton r = Y_equals . head . range_to_list $ r
slice_plane (Dim3D _ _ r) | singleton r = Z_equals . head . range_to_list $ r
slice_plane _                           = error "slice_plane: no singleton dimension"

singleton :: Ord a => Sampling a -> Bool 
singleton (Single _)      = True
singleton (Range f t)     = f == t
singleton (Sampled f s t) = f == t || f == s || s > t

plane_points :: Dimension -> [GL.Vertex3 GL.GLfloat]
plane_points dim
    = case slice_plane dim of
        X_equals v -> [coord v cy cz | cz <- rng_z, cy <- rng_y]
        Y_equals v -> [coord cx v cz | cz <- rng_z, cx <- rng_x]
        Z_equals v -> [coord cx cy v | cy <- rng_y, cx <- rng_x]
      where
        rng_z = range_to_list . dim3d_z $ dim
        rng_y = range_to_list . dim3d_y $ dim
        rng_x = range_to_list . dim3d_x $ dim
        coord x y z = GL.Vertex3 (toFloat x) (toFloat y) (toFloat z)
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


-- hedgehog (first derivative, across the cell)
{-
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
