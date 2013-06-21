
{-# LANGUAGE TypeFamilies, FlexibleContexts, BangPatterns, ScopedTypeVariables, MagicHash #-}

-- Algorithms that are generic wrt type of underlying grid topology

module Algorithms (iso, iso3D) where

import Dataset
import CellTypes
import Control.Applicative
import Maths
import RectGrid

import Graphics.Rendering.OpenGL.GL(Vertex3(..))
import Data.Array.Repa (Array(..), Shape, DIM3, extent, (!), (:.)(..), Z(..), DIM3)
--import Data.Array.Parallel.Unlifted (Elt)
import Graphics.Rendering.OpenGL.GL (GLfloat, Vertex3)

import GHC.Prim
import System.IO.Unsafe

-- Generate the isosurface at a given threshold. Note that the surface is 
-- returned implicitly as a list of points - adjacent groups of three points
-- define the triangles of the surface.
-- (A list of points rather than triangles is used, as it matches better the
-- interface to OpenGL).

{-# SPECIALISE iso3D :: (IsoCell DIM3 Cell_8 Vertex3) => Double -> Grid3D Double -> [[Vertex3 GLfloat]]
 #-}
--iso3D :: Double -> Grid3D Double -> [[Vertex3 GLfloat]]
iso3D th grid = iso' 0 0 0
                where
                    arr = values grid
                    (Z :. zsz :. ysz :. xsz) = shape grid
                    iso' !k !j !i 
                      | k == zsz-1 = []
                      | j == ysz-1 = iso' (k+1) 0 0
                      | i == xsz-1 = iso' k (j+1) 0
                      | otherwise  = let !i1 = i + 1
                                         !j1 = j + 1
                                         !k1 = k + 1
                                         !av = arr ! (Z :. k  :. j  :. i )
                                         !bv = arr ! (Z :. k  :. j  :. i1)
                                         !cv = arr ! (Z :. k  :. j1 :. i1)
                                         !dv = arr ! (Z :. k  :. j1 :. i )
                                         !ev = arr ! (Z :. k1 :. j  :. i )
                                         !fv = arr ! (Z :. k1 :. j  :. i1)
                                         !gv = arr ! (Z :. k1 :. j1 :. i1)
                                         !hv = arr ! (Z :. k1 :. j1 :. i )
                                         ir  :: GLfloat = realToFrac i
                                         jr  :: GLfloat = realToFrac j
                                         kr  :: GLfloat = realToFrac k
                                         i1r :: GLfloat = realToFrac i1
                                         j1r :: GLfloat = realToFrac j1
                                         k1r :: GLfloat = realToFrac k1
                                         ts = mc_case $ Cell_8 (av > th) (bv > th) (cv > th) (dv > th) (ev > th) (fv > th) (gv > th) (hv > th)   
                                     in  
                                         if ts == [] then iso' k j (i+1) 
                                         else (map (surf_geom th (Cell_8 av bv cv dv ev fv gv hv) 
                                                                 (Cell_8 (Vertex3 ir  jr  kr )
                                                                         (Vertex3 i1r jr  kr )
                                                                         (Vertex3 i1r j1r kr )
                                                                         (Vertex3 ir  j1r kr )
                                                                         (Vertex3 ir  jr  k1r)
                                                                         (Vertex3 i1r jr  k1r)
                                                                         (Vertex3 i1r j1r k1r)
                                                                         (Vertex3 ir  j1r k1r)
                                                                 )) ts) : iso' k j (i+1) 
                                          

{-# SPECIALISE iso :: (IsoCell DIM3 Cell_8 Vertex3) => Double -> (Data.Array.Repa.Array DIM3 Double) -> [[Vertex3 GLfloat]]
 #-}
iso th arr = filter (not . null) . map (surf th arr) $ (icells $ extent arr) 
{-# INLINE iso #-}

                 
{-# SPECIALISE surf :: (IsoCell DIM3 Cell_8 Vertex3) => Double -> (Data.Array.Repa.Array DIM3 Double) -> Cell_8 DIM3 -> [Vertex3 GLfloat]
 #-}
surf th arr c = map mkgeom . mc_case . fmap (>th) $ sample
                where sample = fmap (arr !) c
                      mkgeom = surf_geom th sample (fmap iverts c)
{-# INLINE surf #-}

{-# SPECIALISE surf_geom :: Double -> Cell_8 Double -> Cell_8 (Vertex3 GLfloat) -> (MyVertex,MyVertex) -> Vertex3 GLfloat
 #-}
surf_geom th sample geom (v0,v1)
    = interp (inv_interp th samp_0 samp_1) geom_0 geom_1
      where
          samp_0 = select v0 sample
          samp_1 = select v1 sample
          geom_0 = select v0 geom
          geom_1 = select v1 geom
{-# INLINE surf_geom #-}

