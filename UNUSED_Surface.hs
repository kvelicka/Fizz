{-# LANGUAGE PatternSignatures #-}

{- Surface.hs: Simple streaming version of marching cubes.
   This is a simplified version of our generic visualization
   framework, specialized to extracting a single surface
   from a 3D volume taken from the IEEE Visualization Contest
   2008 datasets.  Datasets are assumed to be 150x62x62, i.e.
   downsampled from the contest 600x248x248.
   They are stored using a binary fixed precision format;
   the filename is assumed to have a ".dat" suffix.

   To use the program, 
   (i) compile with ghc: ghc --make -fglasgow-exts Surface
   (ii) execute using: ./Surface filename threshold
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
import CaseTable

-- 2D square cells --------------------------------------------

type Coord = Vertex3 (GLfloat)

data Vertex = A | B | C | D | E | F | G | H deriving (Eq,Show,Enum)

data Cube a = Cube a a a a a a a a deriving (Eq,Ord,Show)

select :: Vertex -> Cube a -> a
select n (Cube a b c d e f g h)
    = case n of
        A  -> a
        B  -> b
        C  -> c
        D  -> d
        E  -> e
        F  -> f
        G  -> g
        H  -> h

-- The "marching cubes" case table, implemented as a map from
-- cube markings to the list of edge pairs that should be 
-- connected to approximate the intersection of the surface
-- with the cell.
mc_case :: Cube Bool -> [(Vertex, Vertex)]
mc_case 
    = (table !)
      where
          table = fromList $ map buildCase $ (markings cube)
          buildCase :: [Vertex] -> (Cube Bool, [(Vertex,Vertex)])
          buildCase mvs
                = ( marked mvs, map from_edge $ concat $ cell_case_verts cube mvs )
          from_edge :: Edge Vertex -> (Vertex,Vertex)
          from_edge (Edge a b) = (a,b)
          marked vs = Cube (p A) (p B) (p C) (p D) (p E) (p F) (p G) (p H)
                      where
                          p x = x `elem` vs
          cube = mkPolyTope [ [A,B,C,D], [E,F,G,H], [A,B,F,E]
                            , [C,D,H,G], [A,D,H,E], [B,C,G,F] ]


mark :: (a -> Bool) -> Cube a -> Cube Bool
mark p (Cube a b c d e f g h) = Cube (p a) (p b) (p c) (p d) (p e) (p f) (p g) (p h)


-- Cubic grid ------------------------------------------------

-- Given the size of a dataset (number of points in each dimension)
-- and a stream of samples, construct a stream of cells.

cubicGrid :: (Int,Int,Int) -> [a] -> [Cube a]
cubicGrid (xdim, ydim, zdim)
    = discontinuities (0,0,0) . make_cells
      where
          make_cells stream = zipWith8 Cube                        stream
                                             (drop 1               stream)
                                             (drop (xdim+1)        stream)
                                             (drop xdim            stream)
                                             (drop plane           stream)
                                             (drop (plane+1)       stream)
                                             (drop (plane+xdim+1)  stream)
                                             (drop (plane+xdim)    stream)
          plane = xdim*ydim
          discontinuities _ [] = []
          discontinuities (i,j,k) (x:xs)
              | k==zdim-1   = []
              | j==ydim-1   =    discontinuities (0,0,k+1) (drop (xdim-1) xs)
              | i==xdim-1   =    discontinuities (0,j+1,k) xs
              | otherwise   = x: discontinuities (i+1,j,k) xs

          zipWith8 fun (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs)
              = (fun a b c d e f g h):zipWith8 fun as bs cs ds es fs gs hs
          zipWith8 _ _ _ _ _ _ _ _ _ = []

-- Contour extraction -----------------------------------------

contour :: Float -> [Cube Float] -> [Cube Coord] -> [[Coord]]
contour th samples geom
    = zipWith (contour_cell th) samples geom

contour_cell :: Float -> Cube Float -> Cube Coord -> [Coord]
contour_cell th sample geom
    = map (cell_edge th sample geom) $ mc_case $ mark (>th) sample

cell_edge :: Float -> Cube Float -> Cube Coord -> (Vertex,Vertex) -> Coord
cell_edge th sample geom (v0,v1)
    = interp (inv_interp th samp_0 samp_1) geom_0 geom_1
      where
          samp_0 = select v0 sample
          samp_1 = select v1 sample
          geom_0 = select v0 geom
          geom_1 = select v1 geom

interp :: Float -> Coord -> Coord -> Coord
interp t (Vertex3 px py pz) (Vertex3 qx qy qz)
    = Vertex3 ((1-t)*px + t*qx) ((1-t)*py + t*qy) ((1-t)*pz + t*qz)

inv_interp :: Float -> Float -> Float -> Float
inv_interp s v1 v2 = (s - v1) / (v2 - v1)


-- Surface drawing --------------------------------------------

draw_contour :: Float -> [Float] -> HsScene
draw_contour t values
    = Group static [ Special $ color (Color3 1.0 0.0 0.0 :: Color3 GLfloat)
                   , Geometry static Triangles [HsGeom_nt $ zip (map make_normal tris) tris]
                   ]
      where
          mkgrid = cubicGrid (150,62,62)
          coords = mkgrid $ [ Vertex3 (float cx) (float cy) (float cz) 
                            | cz <- [0, 4 .. 247]
                            , cy <- [0, 4 .. 247]
                            , cx <- [0, 4 .. 599]
                            ]
          cells  = mkgrid $ values
          float  = fromRational . toRational

          tris   = triples $ concat $ contour t cells coords
          triples (v1:v2:v3:vs) = (v1,v2,v3):(triples vs)
          triples _             = []

 
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
