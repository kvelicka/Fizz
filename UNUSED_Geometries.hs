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