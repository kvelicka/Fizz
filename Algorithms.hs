{-# LANGUAGE BangPatterns #-}

-- Algorithms that are generic wrt type of underlying grid topology

module Algorithms (isosurface) where

import Control.Parallel.Strategies

import CellTypes
import Maths

-- Generate the isosurface at a given threshold. Note that the surface is 
-- returned implicitly as a list of points - adjacent groups of three points
-- define the triangles of the surface.
-- (A list of points rather than triangles is used, as it matches better the
-- interface to OpenGL).

isosurface :: (Interp a, InvInterp a, Interp g, Cell c v, Enum v) =>
    a -> Stream c v a -> Stream c v g -> [[g]]
isosurface threshold samples geometry
    = (map (surfCell threshold) $
      zip (stream samples) (stream geometry)) `using`
      (parListChunk 2048 rseq)

surfCell :: (Interp a, InvInterp a, Interp g, Cell c v) =>
  a -> (c a, c g) -> [g]
surfCell threshold (sample, geom)
    = map (surfGeom threshold sample geom) $ 
          mcCase $ fmap (>threshold) sample

surfGeom :: (Interp a, InvInterp a, Interp g, Cell c v, Enum v) =>
    a -> c a -> c g -> (v,v) -> g
surfGeom threshold sample geom (v0, v1)
    = interp (invInterp threshold samp_0 samp_1) geom_0 geom_1
      where
          !samp_0 = select v0 sample 
          !samp_1 = select v1 sample
          !geom_0 = select v0 geom
          !geom_1 = select v1 geom

