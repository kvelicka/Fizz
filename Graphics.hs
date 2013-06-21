{- Visual mapping

   The first section of code supports the visualization techniques available.
   Not all of these were used in answering the IEEE Visualization contest
   questions, for example we did no work with vector visualization, but have
   retained the hedgehog code.
-}

module Graphics where

import Graphics.Rendering.OpenGL.GL
import qualified Graphics.UI.GLUT as GLUT
import Data.List (foldl')
import Render
import Colour
import Maths
import CellTypes
import RectGrid

import System.IO.Unsafe
import System.Exit
---------------------------------------------------------------------
-- 1. Isosurfacing: construct a scene from lists of vertex geometry 
-- and vertex colour.  For the contest, surface colour is uniform,
-- but the interface below supports probing.
---------------------------------------------------------------------

surface_geom ::  [Vertex3 GLfloat] -> [Color4 GLfloat] -> HsScene
surface_geom verts cols
    = Group static $ geom
      where
          --verts = concat vs
          geom = [ Special $ blend      $= Disabled
	         , Special $ depthMask  $= Enabled
		 , Special $ render_mode Enabled 1.0 Fill
                 , Special $ color (head cols)
                 --, Special $ (putStrLn ("Result "++(show $ s verts 0))) >> exitWith ExitSuccess  
                 --, Special $ (putStrLn ("Results "++(show $ length vs) ++ " " ++ (show $ length verts)))
                 , Geometry static Triangles $ [HsGeom_nt $ normt] -- map (HsGeom_nt . normt) verts  -- 
                 --, Geometry static Triangles $ [HsGeom_v verts] -- map (HsGeom_nt . normt) verts  -- 
                 ]
          -- s [] acc = acc        
          -- s (a:as) acc = s as (t a acc)
          -- t [] acc = acc
          -- t ((Vertex3 x _ _):ts) acc = t ts (x+acc)
          normt = map (\g -> (make_normal g, g)) . triples  $ verts
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
{-# INLINE surface_geom #-}
             

---------------------------------------------------------------------
-- 2.  2D contour lines: construct a scene from a set of lines using
-- a given colour.
---------------------------------------------------------------------



contour_geom ::  [[Vertex3 GLfloat]] -> [Color4 GLfloat] -> HsScene
contour_geom levs cols
    = Group static $ [Special $ clear [DepthBuffer]] ++ zipWith contour levs cols
      where
          contour lev col = Geometry static Lines [HsGeom_cv col lev]


---------------------------------------------------------------------
-- 3.  Axes.  Produce a set of coordinate axes with markers every 10
-- points along.
---------------------------------------------------------------------

axes :: GLfloat -> GLfloat -> GLfloat -> HsScene
axes xlen ylen zlen 
    = Geometry static Lines $ [x_lines, y_lines, z_lines]
      where 
          x_lines = HsGeom_cv (Color3 1.0 0.0 0.0 :: Color3 GLfloat) $
                    [ Vertex3 0.0 0.0 0.0, Vertex3 xlen 0.0 0.0] ++
                    (concat $ [[Vertex3 x 0.0 0.0, Vertex3 x 5.0 0.0] | x <- [10.0, 20.0 .. xlen]])
          y_lines = HsGeom_cv (Color3 0.0 1.0 0.0 :: Color3 GLfloat) $
                    [ Vertex3 0.0 0.0 0.0, Vertex3 0.0 ylen 0.0] ++
                    (concat $ [[Vertex3 0.0 y 0.0, Vertex3 5.0 y 0.0] | y <- [10.0, 20.0 .. ylen]])
          z_lines = HsGeom_cv (Color3 0.0 0.0 1.0 :: Color3 GLfloat) $
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

fplane (r1:r2:rs) = HsGeom_vc (qstrip r1 r2 []) : fplane (r2:rs)
fplane _          = []

plane :: Color c => [[(HsVertex, c)]] -> HsScene
plane vs = Geometry static QuadStrip $ fplane vs 


{-
interleave :: ([a], [a]) -> [a]
interleave ([],    _) = []
interleave (a:as, bs) = a: interleave (bs, as)

plane :: Color c => [[(HsVertex, c)]] -> HsScene
plane vs = Geometry static QuadStrip $ map (HsGeom_vc . interleave) $ zip vs (tail vs)
-}

planes :: Color c => [[[(HsVertex, c)]]] -> HsScene
planes vs = (Group static $ map plane vs)


-- strict map
smap f [] = []
smap f (x:xs) = let fx = f $! x in fx:smap f xs

volume_geom :: (Int, Int, Int) -> [Vertex3 GLfloat] -> [Color4 GLfloat] -> HsScene
volume_geom (xsz,ysz,zsz) gs cs
    = Group static $ transp ++ [Switch (planes yzplanes) (planes xzplanes) (planes xyplanes)]
      where
          transp   = [ Special $ blend      $= Enabled
	             , Special $ depthMask  $= Disabled
                     ]
          xyrows   = splitInto xsz voxels
          xyplanes = splitInto ysz xyrows
          yzrows   = foldl' (zipWith $ flip (:)) (replicate xsz []) xyrows
          yzplanes = smap (splitInto ysz) yzrows
          xzplanes = foldl' (zipWith $ flip (:)) (replicate ysz []) xyplanes
          voxels   = zip gs cs


---------------------------------------------------------------------
-- 5.  Scatterplot.  Given three lists of sample values scaled into
-- the range 0..128, we generate a 3D scatterplot with a single point
-- in the plot representing a cell having that combination of sample
-- values.
---------------------------------------------------------------------

scatterplot :: [GLfloat] -> [GLfloat] -> [GLfloat] -> HsScene
scatterplot vas vbs vcs
    = compile Nothing $ Group static (points ++ planes)
      where
          nbs = 248.0
          planes = [ make_plane (Normal3 0.0 1.0 0.0) (Color3 0.2 0.2 0.2 :: Color3 GLfloat) $
                                                      [ Vertex3 0.0 0.0 0.0
                                                      , Vertex3 nbs 0.0 0.0
                                                      , Vertex3 nbs nbs 0.0
                                                      , Vertex3 0.0 nbs 0.0
                                                      ]
                   , make_plane (Normal3 1.0 0.0 0.0) (Color3 0.2 0.2 0.2 :: Color3 GLfloat) $
                                                      [ Vertex3 0.0 0.0 0.0
                                                      , Vertex3 0.0 0.0 nbs
                                                      , Vertex3 0.0 nbs nbs
                                                      , Vertex3 0.0 nbs 0.0
                                                      ]
                   , make_plane (Normal3 0.0 0.0 1.0) (Color3 0.2 0.2 0.2 :: Color3 GLfloat) $
                                                      [ Vertex3 0.0 0.0 0.0
                                                      , Vertex3 nbs 0.0 0.0
                                                      , Vertex3 nbs 0.0 nbs
                                                      , Vertex3 0.0 0.0 nbs
                                                      ]
                   ]
          points = [ Special $ blend  $= Disabled
                   , Special $ depthMask  $= Enabled
                   , Geometry static Points [HsGeom_cv (Color3 1.0 1.0 1.0 :: Color3 GLfloat) $
                     zipWith3 Vertex3 vas vbs vcs]
                   ]

make_plane norm col vs 
    = Group static $
      [ Special $ color col
      , Geometry static Quads [HsGeom_nv norm vs]
      ]



{- Bits and pieces.  -}

-- An interactor to provide frame-by-frame control over animations.
-- The following code provides for start/go, forward frame (>), 
-- and backward frame (<).
{-
anim_control :: HsHandler HsMovie
anim_control 
    = Just react
      where
          react Timer (HsMovie True [h]  hsos) = HsMovie True (reverse $ h:hsos) []
          react Timer (HsMovie True (h:hs) os) = HsMovie True hs (h:os)
          react (KeyMouse (GLUT.Char 's') GLUT.Down _ _) mov = mov{playing = False}
          react (KeyMouse (GLUT.Char 'g') GLUT.Down _ _) mov = mov{playing = True}
          react (KeyMouse (GLUT.Char '<') GLUT.Down _ _) (HsMovie f n o) 
              = case o of
                  []     -> let n' = reverse n in HsMovie f [head n'] (tail n')
                  (o:os) -> HsMovie f (o:n) os
          react (KeyMouse (GLUT.Char '>') GLUT.Down _ _) (HsMovie f n o)
              = case n of
                  []     -> let o' = reverse o      in HsMovie f o' []
                  [n1]   -> let o' = reverse (n1:o) in HsMovie f o' []
                  (n:ns) -> HsMovie f ns (n:o)
          react _ scenes = scenes
-}



---------------------------------------------------------------------
-- Probing.  
-- Given a scalar field as a list of cells, and a separate list of 
-- coordinates within cells, use the latter to probe the former.
---------------------------------------------------------------------

probe :: (InvInterp g, Interp a) => Cells Cell_8 MyVertex (Vertex3 g, a) -> [[Vertex3 g]] -> [[(Vertex3 g, a)]]
probe ds points = zipWith probe' (cells ds) points

probe' :: (InvInterp g, Interp a) => Cell_8 (Vertex3 g, a) -> [Vertex3 g] -> [(Vertex3 g, a)]
probe' cell points = zip points $ map (tri_linear cell) points

tri_linear :: (InvInterp g, Interp a) => Cell_8 (Vertex3 g, a) -> Vertex3 g -> a
tri_linear cell (Vertex3 px py pz)
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
          u = inv_interp px ax gx
          v = inv_interp py ay gy
          w = inv_interp pz az gz
          field x = snd $ select x cell
          point x = fst $ select x cell


{-

NB need to check clash with second defn of glyph
   also needs defn of x/y_axis copied.

glyph :: Vertex3 GLfloat -> Vertex3 GLfloat -> HsScene
glyph (Vertex3 cx cy cz) (Vertex3 dx dy dz)
    = Group static $
      [ Transform static $ Translate cx cy cz
      , Transform static $ Rotate (-angB) y_axis
      , Transform static $ Rotate (-angA) x_axis
      , Transform static $ Scale mag mag mag
      , Geometry static Polygon $
                  [ HsGeom_nv n1 [p_b, p_a, apex]
                  , HsGeom_nv n2 [p_b, p_c, apex]
                  , HsGeom_nv n3 [p_c, p_d, apex]
                  , HsGeom_nv n4 [p_d, p_a, apex]
                  , HsGeom_nv n5 [p_d, p_c, p_b, p_a]
                  ]
      ]
      where
          n1 = make_normal (p_a, p_b, apex)
          n2 = make_normal (p_b, p_c, apex)
          n3 = make_normal (p_c, p_d, apex)
          n4 = make_normal (p_d, p_a, apex)
          n5 = Normal3 0.0 0.0 (-1.0)
          apex = Vertex3 0.0 0.0 1.0
          (Vector3 u v w) = normalise $ Vector3 dx dy dz
          mag             = min 1.0 $ sqrt $ dx*dx + dy*dy + dz*dz
          rad             = 0.3
          angA = (180.0/pi) * (acos $ w/d)
          angB = (180.0/pi) * (acos $ d)
          d    = sqrt $ v*v + w*w
          s     = 0.25
          p_a   = Vertex3 ( s) ( s) 0.0
          p_b   = Vertex3 (-s) ( s) 0.0
          p_c   = Vertex3 (-s) (-s) 0.0
          p_d   = Vertex3 ( s) (-s) 0.0
-}

---------------------------------------------------------------------
-- 6.  Hedgehog plot.  
-- NOT USED in the design contest.
-- The code in this section implements 3D hedgehog plots using both
-- oriented lines and glyphs.  We also provide two different methods
-- for deriving gradient information from a scalar field.
---------------------------------------------------------------------
{-
type PureData a   = Dataset Cell_8 Cell.MyVertex a
type GeomData g a = PureData (Vertex3 g, a)

type VectFunc g a = (a,a,a) -> Cell_8 (a,a,a) -> Vertex3 a

-- streaming a hedgehog plot, parameterised on hog function 
hedge :: (a,a,a) -> VectFunc g a -> PureData (a,a,a) -> [Vertex3 a]
hedge dim f (Dataset vf) = map (f dim) vf

-- hedgehog (UVW data, only for origin vertex in cell)
hog, hog' :: (Floating g, Fractional a) => VectFunc g a
hog (rngx,rngy,rngz) cell
    = let (u,v,w) = select Cell.A cell in Vertex3 (4*u/rngx) (4*v/rngy) (4* w/rngz)

-- hedgehog (first derivative, across the cell)
hog' (rngx,rngy,rngz) cell
    = Vertex3 (4* dx/rngx) (4* dy/rngy) (4* dz/rngz)
      where
          dx = sum_samples uf [Cell.B, Cell.C, Cell.F, Cell.G] - sum_samples uf [Cell.A, Cell.D, Cell.E, Cell.H]
          dy = sum_samples vf [Cell.C, Cell.D, Cell.G, Cell.H] - sum_samples vf [Cell.A, Cell.B, Cell.E, Cell.F]
          dz = sum_samples wf [Cell.E, Cell.F, Cell.G, Cell.H] - sum_samples wf [Cell.A, Cell.B, Cell.C, Cell.D]
          sum_samples cf ixs = sum $ map (cf.((flip select) cell)) $ ixs
          uf = \(u,_,_) -> u
          vf = \(_,v,_) -> v
          wf = \(_,_,w) -> w

-- Find a cell center.  Vertex3 is an applicative functor, so we can 
-- simply halve the sum of diagonally opposite points!
--
cell_center :: Cell_8 (Vertex3 GLfloat) -> Vertex3 GLfloat
cell_center cell = fmap (*0.5) $ pure (+) <*> (select Cell.A cell) <*> (select Cell.G cell)

hog_geom :: [Vertex3 GLfloat] -> [Vertex3 GLfloat] -> Color4 GLfloat -> HsScene
hog_geom vecs centers col
    = compile Nothing $ Group static scene
      where
          scene = [ Special $ color col
                  , Geometry static Lines [HsGeom_v verts]
                  ]
          verts = concat $ zipWith lines vecs centers
          lines (Vertex3 dx dy dz) (Vertex3 cx cy cz)
                = [Vertex3 cx cy cz, Vertex3 (cx+dx) (cy+dy) (cz+dz)]


glyph :: [Vertex3 GLfloat] -> [Vertex3 GLfloat] -> Color4 GLfloat -> HsScene
glyph vecs centers col
    = compile Nothing $ 
      Group static $
      [Special $ color col] ++ zipWith arrow vecs centers
      where
          y_axis = Vector3 0.0 1.0 0.0
          x_axis = Vector3 1.0 0.0 0.0
          arrow (Vertex3 dx dy dz) (Vertex3 cx cy cz) 
              = Group static $
                [ Transform static $ Translate cx cy cz
                , Transform static $ Rotate (-angB) y_axis
                , Transform static $ Rotate (-angA) x_axis
                , Transform static $ Scale mag mag mag
                , Geometry static Polygon $
                  [ HsGeom_nv n1 [p_b, p_a, apex]
                  , HsGeom_nv n2 [p_b, p_c, apex]
                  , HsGeom_nv n3 [p_c, p_d, apex]
                  , HsGeom_nv n4 [p_d, p_a, apex]
                  , HsGeom_nv n5 [p_d, p_c, p_b, p_a]
                  ]
                ]
                where
                    n1 = make_normal (p_a, p_b, apex)
                    n2 = make_normal (p_b, p_c, apex)
                    n3 = make_normal (p_c, p_d, apex)
                    n4 = make_normal (p_d, p_a, apex)
                    n5 = Normal3 0.0 0.0 (-1.0)
                    apex = Vertex3 0.0 0.0 1.0
                    (Vector3 u v w) = normalise $ Vector3 dx dy dz
                    mag             = min 1.0 $ sqrt $ dx*dx + dy*dy + dz*dz
                    rad             = 0.3
                    angA = (180.0/pi) * (acos $ w/d)
                    angB = (180.0/pi) * (acos $ d)
                    d    = sqrt $ v*v + w*w
                    s     = 0.25
                    p_a   = Vertex3 ( s) ( s) 0.0
                    p_b   = Vertex3 (-s) ( s) 0.0
                    p_c   = Vertex3 (-s) (-s) 0.0
                    p_d   = Vertex3 ( s) (-s) 0.0

-- Slice the dataset to get a 2D plane, or 1D line, or 0D cell.
slice :: Num g => (Maybe g, Maybe g, Maybe g) -> GeomData g a -> GeomData g a
slice (x,y,z) (Dataset ds) =
    Dataset $ (filter zdim . filter ydim . filter xdim) ds
  where
    dimension dim Nothing  _       = True
    dimension dim (Just n) cell    = dim (fst $ select Cell.A cell) == n
    xdim = dimension (\ (Vertex3 i j k)->i) x
    ydim = dimension (\ (Vertex3 i j k)->j) y
    zdim = dimension (\ (Vertex3 i j k)->k) z

-}
