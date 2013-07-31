-- Algorithms that are generic wrt type of underlying grid topology

module Algorithms (isosurface) where

import Control.Applicative

import CellTypes
import Dataset
import Maths

-- Generate the isosurface at a given threshold. Note that the surface is 
-- returned implicitly as a list of points - adjacent groups of three points
-- define the triangles of the surface.
-- (A list of points rather than triangles is used, as it matches better the
-- interface to OpenGL).

isosurface :: (Interp a, InvInterp a, Interp g, Cell c v, Enum v) =>
    a -> Stream c v a -> Stream c v g -> [[g]]
isosurface th samples geom
    = zipWith (surfCell th) 
              (CellTypes.stream samples) 
              (CellTypes.stream geom)

surfCell :: (Interp a, InvInterp a, Interp g, Cell c v, Enum v) =>
    a -> c a -> c g -> [g]
surfCell th sample geom
    = map (surfGeom th sample geom) $ mcCase $ fmap (>th) sample

surfGeom :: (Interp a, InvInterp a, Interp g, Cell c v, Enum v) =>
    a -> c a -> c g -> (v,v) -> g
surfGeom th sample geom (v0,v1)
    = interp (invInterp th samp_0 samp_1) geom_0 geom_1
      where
          samp_0 = select v0 sample
          samp_1 = select v1 sample
          geom_0 = select v0 geom
          geom_1 = select v1 geom

