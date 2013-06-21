module Geometries where

import Maths
import Dataset
import Control.Applicative
import Graphics.Rendering.OpenGL.GL

-- Instances of geometric systems.
-- 2- and 3-D cartesian systems will be used widely, so are defined here to
-- prevent being re-introduced each time they are needed.  

class Coord2D a where
  x_ :: a v -> v
  y_ :: a v -> v

class Coord2D a => Coord3D a where
  z_ :: a v -> v

instance Coord2D Vertex2 where
  x_ (Vertex2 px _) = px
  y_ (Vertex2 _ py) = py

instance Coord2D Vertex3 where
  x_ (Vertex3 px _ _) = px
  y_ (Vertex3 _ py _) = py

instance Coord3D Vertex3 where
  z_ (Vertex3 _ _ pz) = pz

{-
instance Functor Vertex2 where
  fmap f (Vertex2 x y) = Vertex2 (f x) (f y)

instance Functor Vertex3 where
  fmap f (Vertex3 x y z) = Vertex3 (f x) (f y) (f z)

instance Applicative Vertex2 where
  pure x = Vertex2 x x
  (Vertex2 f1 f2) <*> (Vertex2 v1 v2) = Vertex2 (f1 v1) (f2 v2)

instance Applicative Vertex3 where
  pure x = Vertex3 x x x
  (Vertex3 f1 f2 f3) <*> (Vertex3 v1 v2 v3) = Vertex3 (f1 v1) (f2 v2) (f3 v3)
-}
