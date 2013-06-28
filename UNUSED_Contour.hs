{-# LANGUAGE PatternSignatures #-}

{- Contour.hs: Simple streaming version of marching squares. 
   This is a simplified version of our generic visualization
   framework, specialized to extracting a single contour
   from a 2D slice taken from the IEEE Visualization Contest
   2008 datasets.  Datasets are assumed to be 600x248
   slices, stored using a binary fixed precision format.
   The filename is assumed to have a ".dat" suffix.

   To use the program, 
   (i) compile with ghc: ghc --make -fglasgow-exts Contour
   (ii) execute using: ./Contour filename threshold
-}

module Main where

import System.IO
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as Fast
import Data.Map (Map, fromList, (!))
import Data.List (zipWith4)
import Graphics.Rendering.OpenGL.GL (Vertex3(..), color, Color3(..), PrimitiveMode(..), GLfloat)
import qualified Graphics.UI.GLUT as GLUT

import Render
import AstroData (sampleToFloat, bytesToSamples)


-- 2D square cells --------------------------------------------

type Coord = Vertex3 (GLfloat)

data Vertex = A | B | C | D deriving (Eq,Show,Enum)

data Square a = Square a a a a deriving (Eq,Ord,Show)

select :: Vertex -> Square a -> a
select n (Square a b c d)
    = case n of
        A  -> a
        B  -> b
        C  -> c
        D  -> d

-- The "marching squares" case table, implemented as a map from
-- square markings to the list of edge pairs that should be 
-- connected to approximate the intersection of the contour
-- with the cell.
mc_case :: Square Bool -> [(Vertex, Vertex)]
mc_case marking 
    = table ! marking
      where
          table = fromList [ (Square False False False False, [])
                   , (Square True  False False False, [(A,B), (A,D)])
                   , (Square False True  False False, [(A,B), (B,C)])
                   , (Square True  True  False False, [(B,C), (A,D)])
                   , (Square False False True  False, [(B,C), (C,D)])
                   , (Square True  False True  False, [(A,B), (A,D), (B,C), (C,D)])
                   , (Square False True  True  False, [(A,B), (C,D)])
                   , (Square True  True  True  False, [(D,A), (D,C)])
                   , (Square False False False True,  [(D,A), (D,C)])
                   , (Square True  False False True,  [(A,B), (C,D)])
                   , (Square False True  False True,  [(A,B), (B,C), (C,D), (D,A)])
                   , (Square True  True  False True,  [(B,C), (C,D)])
                   , (Square False False True  True,  [(B,C), (A,D)])
                   , (Square True  False True  True,  [(A,B), (B,C)])
                   , (Square False True  True  True,  [(A,D), (A,B)])
                   , (Square True  True  True  True,  [])
                   ]

mark :: (a -> Bool) -> Square a -> Square Bool
mark p (Square a b c d) = Square (p a) (p b) (p c) (p d)


-- Square grid ------------------------------------------------

-- Given: the size of a dataset in *cells*
--        the stream of point samples
-- construct a stream of cells.

squareGrid :: (Int,Int) -> [a] -> [Square a]
squareGrid (xmax,ymax)
    = discontinuities (0,0) . make_cells
      where
          make_cells stream = zipWith4 Square stream
                                             (drop 1 stream)
                                             (drop (line+1) stream)
                                             (drop line stream)
          line  = xmax+1
          discontinuities _ [] = []
          discontinuities (i,j) (x:xs)
              | j==ymax   = []
              | i==xmax   =    discontinuities (0,j+1) xs
              | otherwise = x: discontinuities (i+1,j) xs


-- Contour extraction -----------------------------------------

contour :: Float -> [Square Float] -> [Square Coord] -> [[Coord]]
contour th samples geom
    = zipWith (contour_cell th) samples geom

contour_cell :: Float -> Square Float -> Square Coord -> [Coord]
contour_cell th sample geom
    = map (cell_edge th sample geom) $ mc_case $ mark (>th) sample

cell_edge :: Float -> Square Float -> Square Coord -> (Vertex,Vertex) -> Coord
cell_edge th sample geom (v0,v1)
    = interp (inv_interp th samp_0 samp_1) geom_0 geom_1
      where
          samp_0 = select v0 sample
          samp_1 = select v1 sample
          geom_0 = select v0 geom
          geom_1 = select v1 geom

interp :: Float -> Coord -> Coord -> Coord
interp t (Vertex3 px py pz) (Vertex3 qx qy qz)
    = Vertex3 ((1-t)*px + t*qx) ((1-t)*py + t*qy) ((1-t)*pz + t*pz)

inv_interp :: Float -> Float -> Float -> Float
inv_interp s v1 v2 = (s - v1) / (v2 - v1)


-- Contour drawing --------------------------------------------

draw_contour :: Float -> [Float] -> HsScene
draw_contour t values
    = Group static [ Special $ color (Color3 1.0 0.0 0.0 :: Color3 GLfloat)
                   , Geometry static Lines [HsGeom_v $ lines]
                   ]
      where
          mkgrid = squareGrid (599, 247)
          coords = mkgrid $ [ Vertex3 cx cy 124.0 | cy <- [0.0 .. 247.0]
                                                  , cx <- [0.0 .. 599.0]
                                                  ]
          cells  = mkgrid $ values
          lines  = concat $ contour t cells coords


read_data :: String -> IO [Float]
read_data filename
    = do { h <- openFile (filename ++ ".dat") ReadMode 
         ; b <- Fast.hGetContents h
         ; return $ map sampleToFloat . bytesToSamples $ b
         }

main :: IO ()
main = do { (_, [filename, th]) <- GLUT.getArgsAndInitialize
          ; dataset <- read_data filename
          ; g <- openGraphics "Contour" (800,600)
          ; addScene g $ [draw_contour (read th) dataset]
          ; GLUT.mainLoop
          }

