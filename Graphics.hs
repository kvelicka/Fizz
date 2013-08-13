module Graphics where

import Data.List (foldl')
import Graphics.Rendering.OpenGL.GL
import qualified Graphics.UI.GLUT as GLUT

import CellTypes
import Maths
import RectGrid
import Render

---------------------------------------------------------------------
-- 1. Isosurfacing: construct a scene from lists of vertex geometry 
-- and vertex colour.  For the contest, surface colour is uniform,
-- but the interface below supports probing.
---------------------------------------------------------------------

surfaceGeom ::  [Vertex3 GLfloat] -> [Color4 GLfloat] -> HsScene
surfaceGeom verts cols
    = Group static $ geom
      where
          geom = [ Special $ blend      $= Disabled
      	         , Special $ depthMask  $= Enabled
		             , Special $ render_mode Enabled 1.0 Fill
                 , Special $ color (head cols)
                 , Geometry static Triangles $ [HsGeomNt $ normt] -- map (HsGeomNt . normt) verts  -- 
                 ]
          normt = map (\g -> (makeNormal g, g)) . triples  $ verts
          {-# INLINE normt #-}
          triples (v1:v2:v3:vs) = (v1,v2,v3):(triples vs)
          triples _             = []
          {-# INLINE triples #-}
          render_mode lt lw pm
           = do { lighting $= lt
                ; light (Light 0) $= lt
                ; lineWidth $= lw
                ; polygonMode $= (pm, pm)
                ; polygonOffsetFill$= lt
                }
{-# INLINE surfaceGeom #-}
           

---------------------------------------------------------------------
-- 2.  2D contour lines: construct a scene from a set of lines using
-- a given colour.
---------------------------------------------------------------------



contourGeom ::  [[Vertex3 GLfloat]] -> [Color4 GLfloat] -> HsScene
contourGeom levs cols
    = Group static $ [Special $ clear [DepthBuffer]] ++ zipWith contour levs cols
      where
          contour lev col = Geometry static Lines [HsGeomCv col lev]


---------------------------------------------------------------------
-- 3.  Axes.  Produce a set of coordinate axes with markers every 10
-- points along.
---------------------------------------------------------------------

axes :: GLfloat -> GLfloat -> GLfloat -> HsScene
axes xlen ylen zlen 
    = Geometry static Lines $ [xLines, yLines, zLines]
      where 
          xLines = HsGeomCv (Color3 1.0 0.0 0.0 :: Color3 GLfloat) $
                    [ Vertex3 0.0 0.0 0.0, Vertex3 xlen 0.0 0.0] ++
                    (concat $ [[Vertex3 x 0.0 0.0, Vertex3 x 5.0 0.0] | x <- [10.0, 20.0 .. xlen]])
          yLines = HsGeomCv (Color3 0.0 1.0 0.0 :: Color3 GLfloat) $
                    [ Vertex3 0.0 0.0 0.0, Vertex3 0.0 ylen 0.0] ++
                    (concat $ [[Vertex3 0.0 y 0.0, Vertex3 5.0 y 0.0] | y <- [10.0, 20.0 .. ylen]])
          zLines = HsGeomCv (Color3 0.0 0.0 1.0 :: Color3 GLfloat) $
                    [ Vertex3 0.0 0.0 0.0, Vertex3 0.0 0.0 zlen] ++
                    (concat $ [[Vertex3 0.0 0.0 z, Vertex3 5.0 0.0 z] | z <- [10.0, 20.0 .. zlen]])

---------------------------------------------------------------------
-- 4.  Pseudo-volume rendering.  From a given list of colours and
-- and coordinates, constructed three sets of axes-aligned planes
-- that when drawn back-to-front provide a cheap form of volume
-- rendering. 
-- NOTE: at present we do not generate the "reversed" planes, so 
-- that from some orientations the volume effect may not be correct.
-- The fix is relatively straightforward.
---------------------------------------------------------------------

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto n vs = take n vs : splitInto n (drop n vs)

qstrip [] [] s = s
qstrip (u1:us) (v1:vs) s = qstrip us vs (u1:v1:s)

fplane (r1:r2:rs) = Cell4c (qstrip r1 r2 []) : fplane (r2:rs)
fplane _          = []

plane :: Color c => [[(HsVertex, c)]] -> HsScene
plane vs = Geometry static QuadStrip $ fplane vs 

planes :: Color c => [[[(HsVertex, c)]]] -> HsScene
planes vs = (Group static $ map plane vs)


-- strict map
smap f [] = []
smap f (x:xs) = let fx = f $! x in fx:smap f xs

volumeGeom :: (Int, Int, Int) -> [Vertex3 GLfloat] -> [Color4 GLfloat] -> HsScene
volumeGeom (xsz,ysz,zsz) gs cs
    = Group static $ transp ++ [Switch cyz cxz cxy]
      where
          transp   = [ Special $ blend      $= Enabled
	                   , Special $ depthMask  $= Disabled
                     ]
          cyz = compile Nothing $ Group static [(planes yzplanes) ]
          cxz = compile Nothing $ Group static [(planes xzplanes) ]
          cxy = compile Nothing $ Group static [(planes xyplanes)]
          xyrows   = splitInto xsz voxels
          xyplanes = splitInto ysz xyrows
          yzrows   = foldl' (zipWith $ flip (:)) (replicate xsz []) xyrows
          yzplanes = smap (splitInto ysz) yzrows
          xzplanes = foldl' (zipWith $ flip (:)) (replicate ysz []) xyplanes
          voxels   = zip gs cs

{- Bits and pieces.  -}

-- An interactor to provide frame-by-frame control over animations.
-- The following code provides for start/go, forward frame (>), 
-- and backward frame (<).

animControl :: HsHandler (Bool, [HsScene], [HsScene])
animControl 
    = Just react
      where
          react Timer (True, [h], hsos) = (True, reverse (h:hsos), [])
          react Timer (True, h:hs, os)  = (True, hs, h:os)
          react (KeyMouse (GLUT.Char 's') GLUT.Down _ _) (_, n, o) = (False, n, o) 
          react (KeyMouse (GLUT.Char 'g') GLUT.Down _ _) (_, n, o) = (True,  n, o) 
          react (KeyMouse (GLUT.Char '<') GLUT.Down _ _) (f, n, o) 
              = case o of
                  []     -> let n' = reverse n in (f, [head n'], tail n')
                  (o:os) -> (f, o:n, os)
          react (KeyMouse (GLUT.Char '>') GLUT.Down _ _) (f, n, o) 
              = case n of
                  []     -> let o' = reverse o      in (f, o', [])
                  [n1]   -> let o' = reverse (n1:o) in (f, o', [])
                  (n:ns) -> (f, ns, n:o)
          react _ scenes = scenes


---------------------------------------------------------------------
-- Probing.  
-- Given a scalar field as a list of cells, and a separate list of 
-- coordinates within cells, use the latter to probe the former.
---------------------------------------------------------------------

probe :: (InvInterp g, Interp a) => Cells Cell8 MyVertex (Vertex3 g, a) -> [[Vertex3 g]] -> [[(Vertex3 g, a)]]
probe ds points = zipWith probe' (cells ds) points

probe' :: (InvInterp g, Interp a) => Cell8 (Vertex3 g, a) -> [Vertex3 g] -> [(Vertex3 g, a)]
probe' cell points = zip points $ map (triLinear cell) points

triLinear :: (InvInterp g, Interp a) => Cell8 (Vertex3 g, a) -> Vertex3 g -> a
triLinear cell (Vertex3 px py pz)
    = interp w abcd efgh
      where
          abcd = interp v ab cd
          efgh = interp v ef gh
          ab   = interp u (field A) (field B)
          cd   = interp u (field D) (field C)
          ef   = interp u (field E) (field F)
          gh   = interp u (field H) (field G)
          (Vertex3 ax ay az) = point A  -- opposite corners
          (Vertex3 gx gy gz) = point G  -- of the cube
          u = invInterp px ax gx
          v = invInterp py ay gy
          w = invInterp pz az gz
          field x = snd $ select x cell
          point x = fst $ select x cell
