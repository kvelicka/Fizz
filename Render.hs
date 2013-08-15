{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
--  Render.hs
--  This is a first attempt at a higher-level, reusable interface to
--  OpenGL's rendering services.  It defines a scene-based model, i.e.
--  objects to be displayed are oganized into a tree structure.  Each
--  node in the tree may also hold a callback that may transform the
--  contents of that node in response to an event.

module Render 
	( HsVertex
	, HsVector
	, HsNormal
	, Viewpoint
	, Event(..)
	, HsHandler
	, static
	, dyn
	, HsTransform(..)
        , HsView
	, HsGeom(..)
	, HsScene(..)
	, compile
	, display
        , render
        , addScene
	, openGraphics
	, makeNormal
        , bbox
        , cross
        , vcross
        , vminus
        , vplus
        , normalise
	)
where

import Data.IORef  (IORef, readIORef, writeIORef, newIORef, modifyIORef)
import Data.List(transpose)
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import System.IO.Unsafe (unsafePerformIO)

type HsVertex = Vertex3 GLfloat
type HsVector = Vector3 GLfloat
type HsNormal = Normal3 GLfloat

type Viewpoint = (Vertex3 GLdouble, Vertex3 GLdouble, Vector3 GLdouble)

-- Events are based on the standard OpenGL callbacks, managing key and
-- mouse button presses, mouse motion, and an "idle" callback invoked
-- when GL has nothing else to do (used for animation).
data Event = KeyMouse Key KeyState Modifiers Position
           | Motion Position
           | Idle
           | Timer
           deriving (Eq, Show)

-- If present, an event handler in a node takes an event and some
-- content to new content.  
type HsHandler a = Maybe (Event -> a -> a)
-- type HsHandler2 a = Maybe (Event -> (a,a) -> (a,a))

static :: HsHandler a; static = Nothing
dyn :: (Event -> a -> a) -> HsHandler a; dyn f  = Just f

-- Events for compiled display lists, however, do not fit this
-- nice pattern, as they need the ID of the compiled list as
-- input, and will in general produce an action (e.g. compiling
-- a new scene into the list) as their output.
type HsCompiledHandler = Maybe (Event -> DisplayList -> IO ())

data HsTransform = Rotate GLfloat HsVector
                 | Scale  GLfloat GLfloat GLfloat
                 | Translate GLfloat GLfloat GLfloat
                 | Compose [HsTransform]

type Extent = (Vertex3 GLfloat, Vertex3 GLfloat)



-- Geometry definitions are defined to try and capture common
-- patterns of geometric specification, e.g. a list of
-- vertices, of vertices and normals (one normal per vertex)
-- etc.
data HsGeom = HsGeomV  [HsVertex]
            | HsGeomVn [(HsVertex, HsNormal)]
            | forall c . Color c => Cell4c [(HsVertex, c)]
            | HsGeomNv HsNormal [HsVertex]
            | forall c . Color c => HsGeomCv c [HsVertex]
            | forall c . Color c => HsGeomCnv c [(HsVertex, HsNormal)]
            | HsGeomNt [(HsNormal, (HsVertex, HsVertex, HsVertex))]

data HsView = HsView 
                 { window_size  :: (Int, Int)
                 , aspect_ratio :: GLdouble
                 , last_event   :: (Int, Int)
                 , bounds       :: Extent
                 , eye    :: Vertex3 GLdouble
                 , cop    :: Vertex3 GLdouble
                 , vup    :: Vector3 GLdouble
                 , angle  :: GLdouble
                 , near_z :: GLdouble
                 , far_z  :: GLdouble
                 , mode   :: Maybe (HsView -> Int -> Int -> HsView)
                 }

-- The structure of a scene: 
data HsScene = Camera    (HsHandler HsView)      HsView HsScene
             | Geometry  (HsHandler [HsGeom])    PrimitiveMode [HsGeom]
             | Transform (HsHandler HsTransform) HsTransform
             | Group     (HsHandler [HsScene])   [HsScene]
             | Compiled  HsCompiledHandler       Extent DisplayList
             | Switch                            HsScene HsScene HsScene
             | Imposter                          HsScene HsScene
             | Animate   (HsHandler (Bool, [HsScene],[HsScene])) Bool [HsScene] [HsScene]
             | Special                           (IO ())


-- Interactor ----------------------------------------------------
--

inter (KeyMouse (MouseButton m) Down modi (Position x y)) v
    = v { last_event = (fromEnum x, fromEnum y)
        , mode = case m of
                   LeftButton   -> case modi of
                                     (Modifiers Down _ _) -> Just pan
                                     (Modifiers _ Down _) -> Just zoom
                                     _                    -> Just spin
                   MiddleButton -> Just pan
                   RightButton  -> Just zoom
                   _            -> Nothing 
        }
inter (KeyMouse (MouseButton m) Up _ (Position x y)) v
    = v { last_event = (fromEnum x, fromEnum y)
        , mode       = Nothing
        }
inter (Motion (Position x y)) v
    = case mode v of 
        Nothing -> v
        Just f  -> f v (fromEnum x) (fromEnum y)
inter (KeyMouse (Char c) Down _ _) e = e
inter (KeyMouse _ _ _ _) e = e
inter Idle e  = e
inter Timer e = e

pan :: HsView -> Int -> Int -> HsView
pan view mx my 
    = let viewP   = eye view
          focus   = cop view
          (lx,ly) = last_event view
          (Vertex3 _ _ focalDepth)   = worldToDisplay view focus
          newPP = displayToWorld view (Vertex3 mx my focalDepth)
          oldPP = displayToWorld view (Vertex3 lx ly focalDepth)
          (Vertex3 dx dy dz) = newPP `vminus` oldPP -- `vminus` newPP
          delta :: Vertex3 GLdouble = Vertex3 (double dx) (double dy) (double dz)
      in
          view { last_event = (mx, my)
               , eye        = viewP `vplus` delta
               , cop        = focus `vplus` delta
               }

zoom :: HsView -> Int -> Int -> HsView
zoom view mx my
    = let lasty :: Int = snd . last_event $ view
          delta :: GLdouble = double $ my - lasty
          cy    :: GLdouble = (double . snd . window_size $ view) / 2.0
          dyf   :: GLdouble = 10.0 * delta / cy
          zmf   :: GLdouble = 1.1 ** dyf
          (Vertex3 fx fy fz) = cop view
          (Vertex3 px py pz) = eye view
          dx    :: GLdouble = fx - px
          dy    :: GLdouble = fy - py
          dz    :: GLdouble = fz - pz
          dist  :: GLdouble = sqrt(dx^2 + dy^2 + dz^2)
          d     :: GLdouble = 1.0/zmf
      in 
         view { eye = Vertex3 (fx - d*dx) (fy - d*dy) (fz - d*dz) 
              , last_event = (mx, my)
              }

spin :: HsView -> Int -> Int -> HsView
spin view mx my
    = let (lastx, lasty) = last_event view
          dx :: Int = mx - lastx
          dy :: Int = my - lasty
          (szx, szy) = window_size view
          delta_elev :: GLdouble = -20.0 / (double szy)
          delta_azim :: GLdouble = -20.0 / (double szx)
          rfx :: GLdouble = (double dx) * delta_azim * 10.0
          rfy :: GLdouble = (double dy) * delta_elev * 10.0
      in
          (elevation rfy) . (azimuth rfx) $ view {last_event = (mx,my)}

azimuth   :: GLdouble -> HsView -> HsView
azimuth ang v
    = let (Vertex3 fx fy fz) = cop v
          trneg = hsTranslate (-fx) (-fy) (-fz)
          rot   = hsRotate ang (vup v)
          trpos = hsTranslate fx fy fz
      in  
          v { eye = trpos . rot . trneg $ (eye v) }


elevation :: GLdouble -> HsView -> HsView
elevation ang v 
    = let (Vertex3 px py pz) = eye v
          (Vertex3 fx fy fz) = cop v
          negdop = Vector3 (px-fx) (py-fy) (pz-fz)
          axis   = (vup v) `cross` negdop
          trneg = hsTranslate (-fx) (-fy) (-fz)
          rot   = hsRotate ang axis
          trpos = hsTranslate fx fy fz 
          dop'  = let (Vertex3 ex ey ez) = eye' in (Vector3 (ex-fx) (ey-fy) (ez-fz))
          eye'  = trpos . rot . trneg $ (eye v)
          vup'  = normalise $ dop' `cross` axis
      in 
          v { vup = vup', eye = eye' }


hsTranslate :: Num a => a -> a -> a -> Vertex3 a -> Vertex3 a
hsTranslate dx dy dz (Vertex3 x y z)
    = Vertex3 (x+dx) (y+dy) (z+dz)


hsRotate :: (Floating a, RealFrac a) => a -> Vector3 a -> Vertex3 a -> Vertex3 a
hsRotate ang (Vector3 ax ay az) (Vertex3 x y z)
    = let angle = ang*pi/180.00
          ax' = double ax
          ay' = double ay
          az' = double az
          w   = cos $ 0.5*angle
          f   = (sin $ 0.5*angle) / (sqrt $ double (ax'*ax' + ay'*ay' + az'*az'))
          xf  = ax' * f
          yf  = ay' * f
          zf  = az' * f

          ww  = w*w
          wx  = w*xf
          wy  = w*yf
          wz  = w*zf

          xx  = xf*xf
          yy  = yf*yf
          zz  = zf*zf

          xy  = xf*xf
          xz  = xf*zf
          yz  = yf*zf

          s   = ww - xx - yy - zz

          m00 = xx*2 + s;
          m10 = (xy + wz)*2;
          m20 = (xz - wy)*2;

          m01 = (xy - wz)*2;
          m11 = yy*2 + s;
          m21 = (yz + wx)*2;

          m02 = (xz + wy)*2;
          m12 = (yz - wx)*2;
          m22 = zz*2 + s;
      in
          Vertex3 (m00*x + m01*y + m02*z)
                  (m10*x + m11*y + m12*z)
                  (m20*x + m21*y + m22*z)

double :: (Real a, Fractional b) => a -> b
double = fromRational . toRational



-- Rendering -----------------------------------------------------
--
-- Display a scene; this function probably should not be exported
-- its intended to be used by the callback setup.  It is assumed
-- that rendering uses the depth and colour buffers - other
-- combinations could be handled by parameterization.
display :: IORef HsScene -> IO ()
display root = readIORef root >>= render

{-# NOINLINE vpn #-} 


vpn :: IORef (Vertex3 GLdouble)
vpn = unsafePerformIO(return =<< newIORef (Vertex3 0.0 0.0 0.0 :: Vertex3 GLdouble))

moving :: IORef Bool
moving = unsafePerformIO(return =<< newIORef False)


-- Render
render :: HsScene -> IO ()
render (Camera _ view scene)  = do { clear [DepthBuffer, ColorBuffer]
                                   ; loadIdentity
                                   ; lookAt (eye view) (cop view) (vup view)
                                   ; writeIORef vpn $ (eye view) `vminus` (cop view)
                                   ; case (mode view) of
                                       Just _  -> writeIORef moving True
                                       _       -> writeIORef moving False
                                   ; let (Vertex3 x y z) = eye view
                                   ; position (Light 0) $= Vertex4 1.0 1.0 1.0 0.0
                                   ; render scene
                                   ; exitWith ExitSuccess
                                   ; swapBuffers
                                   ; flush
                                   }
                              where
                                  f = fromRational . toRational . (*2.0)

render (Geometry _ mode gs) = mapM_ (\g -> renderPrimitive mode $ process g) gs
render (Transform _ t)      = apply t
render (Group _ g)          = preservingMatrix (mapM_ render g)
render (Compiled _ _ dl)    = callList dl
render (Switch scx scy scz) = do { Vertex3 ex ey ez <- readIORef vpn
                                 ; let abx = abs ex
                                 ; let aby = abs ey
                                 ; let abz = abs ez
                                 ; if abx > aby
                                    then if abz > abx
                                         then render scz    -- not called when compiled with -O2
                                         else render scx
                                    else if abz > aby
                                         then render scz    -- not called when compiled with -O2
                                         else render scy
                                 }
render (Imposter st dy)     = do { upd <- readIORef moving
                                 ; if upd
                                   then render dy
                                   else render st
                                 }
render (Special act)        = act
render (Animate _ _ (s:_) _)  = render s

process :: HsGeom -> IO()
process (HsGeomV vs)      = mapM_ vertex vs
process (HsGeomVn vns)    = mapM_ (\(v,n) -> normal n >> vertex v) vns 
process (Cell4c vcs)    = mapM_ (\(v,c) -> color c  >> vertex v) vcs
process (HsGeomNv n vs)   = normal n >> mapM_ vertex vs
process (HsGeomCnv c vns) = color c >> (mapM_ (\(v,n) -> normal n >> vertex v) vns)
process (HsGeomCv c vs)   = color c >> mapM_ vertex vs
process (HsGeomNt nts)    = mapM_ tri nts
                             where
                                tri (n,(v1,v2,v3)) = normal n >> vertex v1 >> vertex v2 >> vertex v3

apply :: HsTransform -> IO()
apply (Rotate ang vec)     = rotate ang vec
apply (Scale sx sy sz)     = scale sx sy sz
apply (Translate tx ty tz) = translate (Vector3 tx ty tz)
apply (Compose ts)         = mapM_ apply ts

-- Compile a scene, returning a "Compiled" scene node
compile :: HsCompiledHandler -> HsScene -> HsScene
compile cb scene = unsafePerformIO $
                   do { [dlist] <- genObjectNames 1
                      ; defineList dlist Compile (render scene)
                      ; return (Compiled cb (extent scene) dlist)
                      }



-- Extent handling -----------------------------------------------
--
-- An extent is a cubic volume that bounds the geometry of a scene.
-- Functions are defined for finding minima and maxima of pairs
-- and lists of vertices.

vertMax :: Vertex3 GLfloat = Vertex3   1.0e20    1.0e20    1.0e20
vertMin :: Vertex3 GLfloat = Vertex3 (-1.0e20) (-1.0e20) (-1.0e20)

vertLower :: Ord a => Vertex3 a -> Vertex3 a -> Vertex3 a
vertLower (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) 
    = Vertex3 (min x1 x2) (min y1 y2) (min z1 z2)

vertUpper :: Ord a => Vertex3 a -> Vertex3 a -> Vertex3 a
vertUpper (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2)
    = Vertex3 (max x1 x2) (max y1 y2) (max z1 z2)

vertLbound :: [Vertex3 GLfloat] -> Vertex3 GLfloat
vertLbound = foldr vertLower vertMax

vertUbound :: [Vertex3 GLfloat] -> Vertex3 GLfloat
vertUbound = foldr vertUpper vertMin

vertExtent :: [Vertex3 GLfloat] -> Extent
vertExtent vs = foldr comp (vertMax, vertMin) vs
                 where
                     comp v (minv, maxv) = (vertLower v minv, vertUpper v maxv)

collapse :: [Extent] -> Extent
collapse es 
    = let (mins, maxs) = unzip es in (vertLbound mins, vertUbound maxs)

-- Compute the extent of a scene.  Note that for a compiled scene,
-- we compute and store the extent at the time the scene is compiled.
extent :: HsScene -> Extent
extent (Camera _ _ scene) =   extent scene
extent (Geometry _ _ gs)  =   collapse $ map gExtent gs
extent (Transform _ t)    =   (vertMax, vertMin)
extent (Group _ gs)       =   collapse $ map extent gs
extent (Compiled _ ex _)  =   ex
extent (Special act)      =   (vertMax, vertMin)
extent (Imposter st dy)   =   collapse [extent st, extent dy]
extent (Switch sx sy sz)  =   collapse $ map extent [sx, sy, sz]
extent (Animate _ _ (s:_) _)= extent s


gExtent :: HsGeom -> Extent
gExtent (HsGeomV vs)      = vertExtent vs
gExtent (HsGeomVn vns)    = vertExtent (map fst vns)
gExtent (Cell4c vcs)    = vertExtent (map fst vcs)
gExtent (HsGeomNv _ vs)   = vertExtent vs
gExtent (HsGeomCv _ vs)   = vertExtent vs
gExtent (HsGeomCnv _ vns) = vertExtent (map fst vns)
gExtent (HsGeomNt nvvvs)  = vertExtent (concat $ map (\(_, (v1,v2,v3)) -> [v1,v2,v3]) nvvvs)


-- Event Handling ------------------------------------------------
--
-- When an event happens, we perform a walk of the
-- scene.  Each node that contains a handler is offered the event,
-- which may result in changes to content. In the case of a group
-- node which contains further scenes, the event is first applied
-- to the group level before being passed down to the children - 
-- this choice is somewhat arbitrary, but if the group-level operation
-- deletes or adds children, it makes less sense to do this after the
-- children have been visited.
handle :: Event -> HsScene -> HsScene
handle e (Camera h view g)   = Camera h (response h e view) (handle e g)
handle e (Geometry h mode g) = Geometry h mode (response h e g)
handle e (Transform h t)     = Transform h (response h e t) 
handle e (Group h g)         = Group h (map (handle e) (response h e g))
handle e (Compiled h x dl)   = case h of
                                 Nothing  -> (Compiled h x dl)
                                 Just act -> unsafePerformIO (act e dl) `seq` (Compiled h x dl)
handle e (Animate h f s1 s2) = let (f',r1,r2) = response h e (f,s1,s2) in Animate h f' r1 r2 
handle e (Imposter a b)      = Imposter (handle e a) (handle e b)
handle e scene               = scene

response :: HsHandler a -> Event -> a -> a
response Nothing _ node  = node
response (Just f) e node = f e node

-- Camera Management ---------------------------------------------
--
-- This function is called when the size and shape of the graphics
-- window.

reshape :: IORef HsScene -> Size -> IO()
reshape g dim@(Size width height) =
  do { (Camera h view s) <- readIORef g
     ; viewport $= ((Position 0 0), dim)
     ; matrixMode $= Projection
     ; loadIdentity
     ; perspective (angle view) ((double width)/(double height)) (near_z view) (far_z view)
     ; matrixMode $= Modelview 0
     ; writeIORef g $ Camera h view{ window_size = (fromEnum width, fromEnum height)
                                   , aspect_ratio = (double width)/(double height)
                                   } s
     ; postRedisplay Nothing
     }

-- Position the camera
calibrate :: HsScene -> Extent -> HsScene
calibrate (Camera h view g) bounds''
  = Camera h view' g
    where
        bounds' = extent g
        (Vertex3 xl yl zl, Vertex3 xu yu zu) = bounds'
        xdif = xu - xl
        ydif = yu - yl
        zdif = zu - zl
        cx = double $ (xu + xl)/2.0
        cy = double $ (yu + yl)/2.0
        cz = double $ (zu + zl)/2.0
        rad0    = sqrt (xdif*xdif + ydif*ydif + zdif*zdif) * 0.5
        radius  = (if rad0 == 0 then 1.0 else rad0)
        d       = double $ radius / (sin (pi/4.0))
        cam_up  = vup view
        cam_vpn = normalise $ vert2vec $ (cop view) `vminus` (eye view)
        new_up  = if (abs (cam_up `dot` cam_vpn)) > 0.999 
                  then let (Vector3 upx upy upz) = cam_up 
                       in (Vector3 (negate upz) upx upy)
                  else cam_up
        view' = view { cop    = Vertex3 cx cy cz
                     , eye    = let Vector3 vx vy vz = cam_vpn
                                in Vertex3 (cx + d*vx) (negate (cy + d*vy)) (negate (cz + d*vz))
                     , vup    = cam_up
                     , bounds = bounds'
                     }


-- Add a scene; the scene objects are added to the top-level group
-- directly beneath the camera.
addScene :: IORef HsScene -> [HsScene] -> IO()
addScene g hs = 
  do { let a = collapse $ map extent hs
     ; modifyIORef g (\(Camera h view (Group g s))
                     -> calibrate (Camera h view (Group g (s++hs))) $ a)
     ; postRedisplay Nothing
     }


-- Setup graphics ------------------------------------------------
--
-- Create a graphics window of a specified size, setting up
-- basic rendering options.

openGraphics :: String -> (Int, Int) -> IO(IORef HsScene)
openGraphics name size@(xsz,ysz) =
  do { GLUT.initialDisplayMode    $= [ GLUT.WithDepthBuffer
                                     , GLUT.DoubleBuffered
                                     , GLUT.RGBAMode ]
     ; GLUT.initialWindowSize     $= Size (toEnum.fromEnum $ xsz) (toEnum.fromEnum $ ysz)
     ; GLUT.initialWindowPosition $= Position 200 100
     ; GLUT.createWindow "Surface Viewer"

     -- shading options
     ; clearColor $= (Color4 0.0 0.0 0.0 0.0)
     ; shadeModel $= Smooth
     ; depthMask  $= Enabled

     ; depthFunc  $= Just Less
     ; frontFace  $= CCW
     ; colorMaterial $= Just (FrontAndBack, Diffuse)
     ; lightModelTwoSide $= Enabled
     ; polygonOffsetFill $= Enabled
     ; polygonSmooth     $= Enabled
     ; lineSmooth        $= Enabled


     -- blending
     ; blendEquation $= FuncAdd
     ; blendFunc     $= (SrcAlpha, OneMinusSrcAlpha)
     ; blend         $= Disabled
     ; hint LineSmooth $= DontCare
     ; hint PolygonSmooth $= Fastest

     -- lights ....
     ; lighting           $= Enabled
     ; light (Light 0)    $= Enabled
     ; position (Light 0) $= (Vertex4 800.0 124.0 124.0 1.0)
     ; ambient (Light 0)  $= (Color4 0.8 0.8 0.8 1.0)
     ; diffuse (Light 0)  $= (Color4 0.8 0.8 0.8 1.0)

     -- camera .... default settings, most will be overwritten.
     ; let view = HsView { window_size = size
                         , aspect_ratio = (double xsz)/(double ysz)
                         , last_event  = (0,0)
                         , bounds      = (vertMax, vertMin)
                         , eye   = Vertex3 30.0 30.0 30.0
                         , cop   = Vertex3 0.0 0.0 0.0
                         , vup   = Vector3 0.0 1.0 0.0
                         , mode  = Nothing
                         , near_z = 0.1
                         , far_z  = 1500.0
                         , angle  = 90.0
                         }
     ; let stub = Group static []
     ; root <- newIORef (Camera (Just inter) view stub)

     ; reshape root (Size (toEnum xsz) (toEnum ysz))

     ; displayCallback       $= display root
     ; keyboardMouseCallback $= (Just $ \k s m p -> kbfilter root k s m p)
     ; motionCallback        $= (Just $ \pos -> walk root (Motion pos))
     ; reshapeCallback       $= (Just $ \size -> reshape root size)
     ; idleCallback          $= (Just $ walk root Idle)
     ; addTimerCallback 200 (walkAndReset root)
     ; return root
     }
  where
    kbfilter root c s m p 
        | s == Down && c == (Char '\27') = exitWith ExitSuccess
        | otherwise                      = walk root (KeyMouse c s m p)
    walk root event = do { modifyIORef root (handle event)
                         ; postRedisplay Nothing
                         }
    walkAndReset root = (walk root Timer) >> (addTimerCallback 200 $ walkAndReset root)


-- Utility functions ---------------------------------------------
--
-- Final two functions should probably be part of a rendering utilities
-- package built on top of this module.  "makeNormal" constructs a normal
-- to a plane defined by three points, "bbox" constructs a bounding box.

makeNormal :: (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat) -> Normal3 GLfloat
makeNormal (p1, p2, p3)  = let (Vertex3 x1 y1 z1) = p1
                               (Vertex3 x2 y2 z2) = p2
                               (Vertex3 x3 y3 z3) = p3
                               ax = x2 - x1
                               bx = x3 - x1
                               ay = y2 - y1
                               by = y3 - y1
                               az = z2 - z1
                               bz = z3 - z1
                               prodx = ay*bz - az*by
                               prody = ax*bz - az*bx
                               prodz = ax*by - ay*bx
                               norm = sqrt(prodx*prodx + prody*prody + prodz*prodz)
                           in Normal3 (prodx/norm) (prody/norm) (prodz/norm)

bbox :: (Enum a) => a -> a -> a -> HsScene
bbox ln wd ht = Geometry static Lines $
                [ HsGeomCv (Color3 0.0 1.0 0.0 :: Color3 GLfloat) 
                           [ ftl, ftr
                           , fbl, fbr
                           , ftl, fbl
                           , ftr, fbr
                           , btl, btr
                           , bbl, bbr
                           , btl, bbl
                           , btr, bbr
                           , ftl, btl
                           , ftr, btr
                           , fbl, bbl
                           , fbr, bbr
                           ]
                ]
                where
                    ftl = Vertex3    0    0    0
                    ftr = Vertex3 glln    0    0 
                    fbl = Vertex3    0 glwd    0
                    fbr = Vertex3 glln glwd    0
                    btl = Vertex3    0    0 glht
                    btr = Vertex3 glln    0 glht 
                    bbl = Vertex3    0 glwd glht
                    bbr = Vertex3 glln glwd glht
                    glln = fromIntegral . fromEnum $ ln
                    glwd = fromIntegral . fromEnum $ wd
                    glht = fromIntegral . fromEnum $ ht


-- Vector and matrix operations ----------------------------------

vplus :: Num a => Vertex3 a -> Vertex3 a -> Vertex3 a
(Vertex3 x1 y1 z1) `vplus` (Vertex3 x2 y2 z2)
    = Vertex3 (x1+x2) (y1+y2) (z1+z2)

vminus :: Num a => Vertex3 a -> Vertex3 a -> Vertex3 a
(Vertex3 x1 y1 z1) `vminus` (Vertex3 x2 y2 z2)
    = Vertex3 (x1-x2) (y1-y2) (z1-z2)

vcross :: (Num a) => Vertex3 a -> Vertex3 a -> Vector3 a
v1 `vcross` v2 = (vert2vec v1) `cross` (vert2vec v2)

cross :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
(Vector3 x1 y1 z1) `cross` (Vector3 x2 y2 z2)
  = Vector3 (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)

dot :: Num a => Vector3 a -> Vector3 a -> a
(Vector3 x1 y1 z1) `dot` (Vector3 x2 y2 z2) 
  = (x1*x2 + y1*y2 + z1*z2)

vert2vec :: Vertex3 a -> Vector3 a
vert2vec (Vertex3 x y z) = Vector3 x y z

normalise :: (Floating a) => Vector3 a -> Vector3 a
normalise (Vector3 x y z)
  = let sz = sqrt(x*x + y*y + z*z)
    in Vector3 (x/sz) (y/sz) (z/sz)


type HsMatrix = [[GLdouble]]

identity :: HsMatrix
identity = [ [if i == j then 1.0 else 0.0 | i <- [1..4]] | j <- [1..4]]

multVertex :: HsMatrix -> Vertex4 GLdouble -> Vertex4 GLdouble
[r1, r2, r3, r4] `multVertex` (Vertex4 vx vy vz vw) 
  = Vertex4 (r1 `mdot` vs) (r2 `mdot` vs) (r3 `mdot` vs) (r4 `mdot` vs)
    where
        vs = [vx, vy, vz, vw]

mdot :: Num a => [a] -> [a] -> a
a `mdot` b = sum $ zipWith (*) a b

mcat :: HsMatrix -> HsMatrix -> HsMatrix
-- m `mcat` n = [[r `mdot` c | c <- tn] | r <- m] where tn = transpose n
m `mcat` n = [[r `mdot` c | c <- tm] | r <- n] where tm = transpose m

homogenise :: (Floating a, Eq a) => Vertex4 a -> Vertex3 a
homogenise (Vertex4 x y z w)
  | w == 0.0   = Vertex3 (x/w) (y/w) (z/w)
  | otherwise  = Vertex3 x     y     z

invert :: HsMatrix -> HsMatrix
invert m 
  = let det = det4 m
        adj = adjoint m
    in
        if det == 0.0 
        then error "invert: singular matrix!"
        else map (map (/det)) adj

det2 :: Num a => a -> a -> a -> a -> a
det2 a b c d = a * d - b * c

det3 :: Num a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a
det3 a1 a2 a3 b1 b2 b3 c1 c2 c3
  =   a1 * det2 b2 b3 c2 c3
    - b1 * det2 a2 a3 c2 c3
    + c1 * det2 a2 a3 b2 b3

det4 :: HsMatrix -> GLdouble
det4 m
  = let [ [a1, b1, c1, d1], [a2, b2, c2, d2], [a3, b3, c3, d3], [a4, b4, c4, d4]] = m
    in   a2 * det3 b2 b3 b4 c2 c3 c4 d2 d3 d4
       - b1 * det3 a2 a3 a4 c2 c3 c4 d2 d3 d4
       + c1 * det3 a2 a3 a4 b2 b3 b4 d2 d3 d4
       - d1 * det3 a2 a3 a4 b2 b3 b4 c2 c3 c4


adjoint :: HsMatrix -> HsMatrix
adjoint m
  = let [ [a1, b1, c1, d1], [a2, b2, c2, d2], [a3, b3, c3, d3], [a4, b4, c4, d4]] = m
    in [ [ det3 b2 b3 b4 c2 c3 c4 d2 d3 d4, det3 a2 a3 a4 c2 c3 c4 d2 d3 d4
         , det3 a2 a3 a4 b2 b3 b4 d2 d3 d4, det3 a2 a3 a4 b2 b3 b4 c2 c3 c4
         ]
       , [ det3 b1 b3 b4 c1 c3 c4 d1 d3 d4, det3 a1 a3 a4 c1 c3 c4 d1 d3 d4
         , det3 a1 a3 a4 b1 b3 b4 d1 d3 d4, det3 a1 a3 a4 b1 b3 b4 c1 c3 c4
         ]
       , [ det3 b1 b2 b4 c1 c2 c4 d1 d2 d4, det3 a1 a2 a4 c1 c2 c4 d1 d2 d4
         , det3 a1 a2 a4 b1 b2 b4 d1 d2 d4, det3 a1 a2 a4 b1 b2 b4 c1 c2 c4
         ]
       , [ det3 b1 b2 b3 c1 c2 c3 d1 d2 d3, det3 a1 a2 a3 c1 c2 c3 d1 d2 d3
         , det3 a1 a2 a3 b1 b2 b3 d1 d2 d3, det3 a1 a2 a3 b1 b2 b3 c1 c2 c3
         ]
       ]



-- Coordinate transformation utilities ---------------------------
--
-- These are derived from the VTK implementation.

viewToWorld :: HsView -> Vertex3 GLdouble -> Vertex3 GLdouble
viewToWorld view (Vertex3 x y z)
  = let ctm = perspectiveTransformMatrix view 1.0 0.0 1.0
        inv = invert ctm
        p'  = inv `multVertex` (Vertex4 x y z (double 1.0))
    in homogenise p'

worldToView :: HsView -> Vertex3 GLdouble -> Vertex3 GLdouble
worldToView view (Vertex3 x y z)
  = let ctm = perspectiveTransformMatrix view 1.0 0.0 1.0
        wp  = Vertex4 x y z 1.0
    in homogenise $ ctm `multVertex` wp

-- vtkCamera::GetCompositePerspectiveTransformMatrix
perspectiveTransformMatrix :: HsView -> GLdouble -> GLdouble -> GLdouble -> HsMatrix
perspectiveTransformMatrix view asp near far
  =
  perspectiveTransform view asp near far
  `mcat` (setupCamera view)

-- vtkCamera::ComputePerspectiveTransform
perspectiveTransform :: HsView -> GLdouble -> GLdouble -> GLdouble -> HsMatrix
perspectiveTransform view aspect nearz farz
  = let tmp    = tan((angle view)*(pi/180.0)/2.0);
        width  = (near_z view)*tmp*(aspect_ratio view)
        height = (near_z view)*tmp;
        -- 0.0s below assume window center in view space is (0.0, 0.0)
        xmin = (0.0-1.0)*width;
        xmax = (0.0+1.0)*width;
        ymin = (0.0-1.0)*height;
        ymax = (0.0+1.0)*height;
    in 
    identity
     `mcat` adjustZBuf (-1) 1 nearz farz
     `mcat` hsFrustum xmin xmax ymin ymax (near_z view) (far_z view)

-- vtkPerspectiveTransform::Frustum 
hsFrustum :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> HsMatrix
hsFrustum xmin xmax ymin ymax znear zfar
  = [ [ 2.0*znear/(xmax - xmin), 0, 0, 0]
    , [ 0, 2*znear/(ymax - ymin), 0, 0]
    , [ (xmin + xmax)/(xmax - xmin)
      , (ymin + ymax)/(ymax - ymin)
      , -(znear + zfar)/(zfar - znear)
      , -1
      ]
    , [ 0, 0, -2*znear*zfar/(zfar - znear), 0]
    ]

-- vtkPerspectTransform::AdjustZBuffer
adjustZBuf :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> HsMatrix
adjustZBuf zmin0 zmax0 zmin' zmax'
  = [ [1.0, 0.0, 0.0, 0.0]
    , [0.0, 1.0, 0.0, 0.0]
    , [0.0, 0.0, va1, va2]
    , [0.0, 0.0, 0.0, 1.0]
    ]
    where
        va1 = (zmax'-zmin')/(zmax0-zmin0)
        va2 = (zmin' * zmax0 - zmax'*zmin0) / (zmax0 - zmin0)

-- based on vtkCamera::ComputeViewTransform
--      and vtkPerspectiveTransform::SetupCamera
setupCamera :: HsView -> HsMatrix
setupCamera view
  = let vpn = normalise.vert2vec $ (eye view) `vminus` (cop view)
        sideways = normalise $ (vup view) `cross` vpn
        ortho_up = vpn `cross` sideways
        (Vector3 sdwx sdwy sdwz) = sideways
        (Vector3 vpnx vpny vpnz) = vpn
        (Vector3 oupx oupy oupz) = ortho_up

        m0 = [ [sdwx, sdwy, sdwz, 0]
             , [oupx, oupy, oupz, 0]
             , [vpnx, vpny, vpnz, 0]
             , [0   , 0   , 0,    1]
             ]
        (Vertex3 eyex eyey eyez) = eye view
        (Vertex4 tx ty tz _) = m0 `multVertex` (Vertex4 (-eyex) (-eyey) (-eyez) 1.0)
    in 
        [ [sdwx, sdwy, sdwz, tx]
        , [oupx, oupy, oupz, ty]
        , [vpnx, vpny, vpnz, tz]
        , [0   , 0   , 0,    1]
        ]

worldToDisplay :: HsView -> Vertex3 GLdouble -> Vertex3 Int
worldToDisplay v = (viewToDisplay v) . (worldToView v)

displayToWorld :: HsView -> Vertex3 Int -> Vertex3 GLdouble
displayToWorld v = (viewToWorld v) . (displayToView v)

displayToView :: RealFrac a => HsView -> Vertex3 Int -> Vertex3 a
displayToView v (Vertex3 x0 y0 z0) 
    = Vertex3 (double $ aspect*x') (double y') (double z0)
      where
          (sizex, sizey) = window_size v
          aspect = aspect_ratio v
          x' = 2.0 * (double x0)/(double sizex) - 1.0
          y' = 2.0 * (double y0)/(double sizey) - 1.0

viewToDisplay :: RealFrac a => HsView -> Vertex3 a -> Vertex3 Int
viewToDisplay v (Vertex3 x0 y0 z0) 
    = Vertex3 x' y' (floor z0)
      where
          aspect         = double.aspect_ratio $ v
          (sizex, sizey) = window_size v
          x' = floor $ (x0/aspect + 1.0) * ((double sizex)/2.0)
          y' = floor $ (y0/1.0    + 1.0) * ((double sizey)/2.0)
