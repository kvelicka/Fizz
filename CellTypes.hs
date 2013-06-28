{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeFamilies #-}

module CellTypes where

import Control.Applicative
import Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString as BS

import CaseTable
import Dataset
import Maths




class (Functor c, Enum v) => Cell c v | c -> v where
  data Facet c :: * -> *
  select   :: v  -> c a -> a
  mc_case  :: c Bool -> [(v,v)] --[Facet c (v,v)]
--  renderF  :: Facet  -> IO()
  
newtype Stream c v a = Stream { stream :: [c a] }

newtype Cells c v a = Cells { cells :: [c a] } deriving (Show)


-- Pairing allows us to take two datasets (which should have the same
-- dimensionality, but this is not checked), and construct a dataset
-- containing pairs of values.
pair :: (Applicative c, Enum v, Cell c v) => Cells c v a -> Cells c v b -> Cells c v (a,b)
pair (Cells as) (Cells bs) 
  = Cells $ zipWith (\a b -> pure (,) <*> a <*> b) as bs

splitDS :: (Functor c, Enum v, Cell c v) => Cells c v (a,b) -> (Cells c v a, Cells c v b)
splitDS (Cells cs) 
  = (Cells $ map (fmap fst) cs, Cells $ map (fmap snd) cs)

instance (Functor c, Enum v, Cell c v) => Functor (Cells c v) where
  fmap f (Cells d) = Cells (map (fmap f) d)

instance (Applicative c, Enum v, Cell c v) => Applicative (Cells c v) where
  pure f = Cells (repeat (pure f))
  (Cells a) <*> (Cells b) = Cells (zipWith (<*>) a b)


---- test data
data MyVertex = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P
  deriving (Eq,Show,Enum)

icube :: PolyTope Int
icube    = mkPolyTope [ [0,1,2,3], [4,5,6,7], [0,1,4,5]
                      , [2,3,6,7], [0,3,4,7], [1,2,5,6] ]

triangle, square, pentagon :: PolyTope MyVertex -- 2D
triangle = mkPolyTope [ [A,B], [B,C], [A,C]]
square   = mkPolyTope [ [A,B], [B,C], [C,D], [A,D] ]
pentagon = mkPolyTope [ [A,B], [B,C], [C,D], [D,E], [A,E] ]

tetra, cube, triprism, hexprism, pentaprism :: PolyTope MyVertex -- 3D
tetra    = mkPolyTope [ [A,B,C], [A,B,D], [A,C,D], [B,C,D]  ]
cube     = mkPolyTope [ [A,B,C,D], [E,F,G,H], [A,B,F,E]
                      , [C,D,H,G], [A,D,H,E], [B,C,G,F] ]
triprism = mkPolyTope [ [A,B,C], [D,E,F], [A,B,E,D], [A,C,F,D], [B,C,F,E] ]
hexprism = mkPolyTope [ [A,B,C,D,E,F], [G,H,I,J,K,L], [A,B,H,G], [B,C,I,H]
                      , [C,D,J,I], [D,E,K,J], [E,F,L,K], [F,A,G,L] ]
pentaprism = mkPolyTope [ [A,B,C,D,E], [F,G,H,I,J]
                        , [A,B,G,F], [B,C,H,G]
                        , [C,D,I,H], [D,E,J,I], [E,A,F,J] ]

hypercube :: PolyTope MyVertex -- 4D
hypercube= mkPolyTope [ undefined ]
----

-- first arg is scalar
locateCube :: GLfloat -> MyVertex -> GL.Vertex3 GL.GLfloat
locateCube n A = Vertex3  0.0 n   0.0
locateCube n B = Vertex3  n   n   0.0
locateCube n C = Vertex3  n   0.0 0.0
locateCube n D = Vertex3  0.0 0.0 0.0
locateCube n E = Vertex3  0.0 n   n
locateCube n F = Vertex3  n   n   n
locateCube n G = Vertex3  n   0.0 n
locateCube n H = Vertex3  0.0 0.0 n

locateTetra n A = Vertex3  n   (2*n) n
locateTetra n B = Vertex3  0.0  0.0  0.0
locateTetra n C = Vertex3 (2*n) 0.0  0.0
locateTetra n D = Vertex3  n    0.0 (2*n)

locateTriPrism n A = Vertex3   n  (2*n) 0.0
locateTriPrism n B = Vertex3  0.0  0.0  0.0
locateTriPrism n C = Vertex3 (2*n) 0.0  0.0
locateTriPrism n D = Vertex3   n  (2*n)(2*n)
locateTriPrism n E = Vertex3  0.0  0.0 (2*n)
locateTriPrism n F = Vertex3 (2*n) 0.0 (2*n)

locateHexPrism n A = Vertex3  0.0  (2*n)  0.0
locateHexPrism n B = Vertex3   n   (4*n)  0.0
locateHexPrism n C = Vertex3 (3*n) (4*n)  0.0
locateHexPrism n D = Vertex3 (4*n) (2*n)  0.0
locateHexPrism n E = Vertex3 (3*n)  0.0   0.0
locateHexPrism n F = Vertex3   n    0.0   0.0
locateHexPrism n G = Vertex3  0.0  (2*n) (3*n)
locateHexPrism n H = Vertex3   n   (4*n) (3*n)
locateHexPrism n I = Vertex3 (3*n) (4*n) (3*n)
locateHexPrism n J = Vertex3 (4*n) (2*n) (3*n)
locateHexPrism n K = Vertex3 (3*n)  0.0  (3*n)
locateHexPrism n L = Vertex3   n    0.0  (3*n)

locatePentaPrism n A = Vertex3  0.0    (3*n) 0.0
locatePentaPrism n B = Vertex3 (2.5*n) (5*n) 0.0
locatePentaPrism n C = Vertex3 (5*n)   (3*n) 0.0
locatePentaPrism n D = Vertex3 (4*n)    0.0  0.0
locatePentaPrism n E = Vertex3   n      0.0  0.0
locatePentaPrism n F = Vertex3  0.0    (3*n) (4*n)
locatePentaPrism n G = Vertex3 (2.5*n) (5*n) (4*n)
locatePentaPrism n H = Vertex3 (5*n)   (3*n) (4*n)
locatePentaPrism n I = Vertex3 (4*n)    0.0  (4*n)
locatePentaPrism n J = Vertex3   n      0.0  (4*n)
