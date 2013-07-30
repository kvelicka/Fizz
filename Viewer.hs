{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

{- Viewer.hs: a domain-specific language and opengl viewer for
   multi-variate visualization.

   Authors: Rita Borgo & David Duke, University of Leeds, UK
            Colin Runciman & Malcolm Wallace, University of York UK

   Contact: djd@comp.leeds.ac.uk

   Usage: this code can be used either as a stand-alone program, or
   via the GHC interpreter ghci (which comes as part of the standard
   ghc release - see www.haskell.org/ghc/

   If compiled to an executable, you need to set up the "main" function
   toward the end of the file to generate the desired picture, then
   compile using ghc, and run the executable.

   At the command line, start ghci with
   ghci -fglasgow-exts -O2 Viewer.hs +RKS -K90M

   At the ghci command prompt, you can then enter an expression
   for execution as a picture; for example, to generate 
       - a contour plot of the "G" field,
       - for the slice z=124 at full resolution, 
       - with contours spaced at 1000-degree intervals, and 
       - coloured using a red-based Brewer palette, 

   use
       exec $ Contour reds (Sampled 1000 2000 20000) (Use (From (Range 0 599) (Range 0 247) (Single 124) 60 G))

   A number of example visualizations have been pre-defined at the end
   of this file as simple expressions, for example entering "new_fingers" at
   ghci prompt will produce an animation of a superimposed contour and 
   slice over time.

   See the README file and PictureDSL.hs for further details of the picture language.
-}

module Main where

import Data.List (nubBy)

import Debug.Trace (trace)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import System.IO.Unsafe

import AstroData
import Colour
import Dataset
import Graphics
import Maths
import PictureDSL
import Render


-- TOP LEVEL: execute an expression specifying a picture.
-- The picture expression is first traversed to determine 
-- the files required; access to these via ByteStrings or
-- streams of Floats are generated via "read_data" and 
-- returned as a "context" - a file environment - that is
-- then used by the picture evaluator.  This two-stage
-- process means that the interpreter does not need to 
-- make IO calls itself, simplifying the presentation.

-- run this using GHCi

evalView :: (Dataset d) => View d Float -> IO()
evalView view@(source :> picture)  = 
  do { GLUT.initialize "Astro Viewer" [] >> return ()
     ; g <- openGraphics "Scene Viewer" (1000,800)
     ; addScene g $ [Imposter (Group static [axes 600.0 248.0 248.0, evalPicture view]) (bbox 600 248 248) ]
     ; GLUT.mainLoop
     }


-- main: if compiling, you must come up with a Picture expression here

main :: IO ()
main = do { evalView $ anim }

{- The remainder of this file contains examples of picture-generating 
   expressions.  These can either be entered into the ghc command line,
   or inserted into the "main" function as above and then compiled.
-}

surface = (from4 35 G) :> (Surface red (Single 2500))
draw = (from4 35 G) :> Draw [(Surface red (Single 2500)), (Surface blue (Single 10000)), (Surface green (Single 15000))]
anim = (from4 35 G) :> Anim [(Surface red (Single 2500)), (Surface blue (Single 10000)), (Surface green (Single 15000))]
volume = (from4 35 G) :> (Volume vis5D)
slice = (VisData (Range 0 599) (Range 0 247) (Single 124) 15 D) :> (Slice reds)
contour = (VisData (Range 0 599) (Range 0 247) (Single 124) 15 Mv) :> (Contour greens (Sampled 1 500 10001))

{-
radiation_anim       = Anim [ Surface red (Single 20000) (from4 t G)
                            | t <- [0,5..195] ]

shock_radiation_anim =
     let g t = from4 t G in
     Anim [ Draw [ Surface red (Single 20000) (g t)
                 , Surface yellow (Single 3500) (g t) ]
          | t <- [0,5..195] ]

shock_rad_contours =
     let g t = Use (From (Range 0 599) (Range 0 247) (Single 124) t G)
     in
     Anim [ Draw [ Contour mreds (Sampled 18000 200 20000) (g t)
                 , Contour blues (Sampled 1500 500 4000) (g t) ]
          | t <- [0,5..195] ]

turbulence_anim      = Anim [ Contour greens (Sampled 1 500 10001)
                                      (Use (From (Range 0 599)
                                                 (Range 0 247)
                                                 (Single 124)
                                                 t Mv))
                            | t <- [0,5..195] ]

turbulenceStatic =
    Anim [ Contour greens (Sampled 1 500 10001)
                          (Use (From (Range 0 599) (Range 0 247)
                                                   (Single 124) t Mv))
         | t <- [15,20..60] ]

symmetry =
  let sli t s = Use (From (Range 0 599) (Range 0 247) (Single 124) t s)
  in Anim [ Draw [ Contour blues (Sampled 0 1000 20000) (sli t G)
                 , Contour mreds (Sampled 0 0.000004  0.00004)  (sli t H2) ]
          | t <- [0,5..195] ]

fromDavid =
  let sli s = Use (From (Range 0 599) (Range 0 247) (Single 124) 60 s)
  in Draw [ Slice greys  (sli G)
          , Contour blues (Sampled 1.0e-10 1.0e-8   1.0e-6) (sli H2)
          , Contour reds (Sampled 200 400 1000) (sli Mv)
          ]

h2Mv =
  let sli s = Use (From (Range 0 599) (Range 0 247) (Single 124) 60 s)
  in Draw [ Slice mreds  (sli H2)
          , Contour blues (Sampled 200 400 1000) (sli Mv)
          ]

h2xDMv =
  let sli s = Use (From (Range 0 599) (Range 0 247) (Single 124) 60 s)
  in Draw [ Slice mreds  (sli H2xD)
          , Contour blues (Sampled 200 400 1000) (sli Mv)
          ]

hydrogenConcentration =
  let sli t s = Use (From (Range 0 599) (Range 0 247) (Single 124) t s)
  in Anim [  Contour mreds  (Sampled 0 0.02 0.4) (sli t H2xD)
          | t <- [150..170] ]

hydrogenTurbulence =
  let sli t s = Use (From (Range 0 599) (Range 0 247) (Single 124) t s)
  in Anim [  Draw [ Contour mreds   (Sampled 0 0.02 0.4)   (sli t H2xD)
                  , Contour mblues  (Sampled 200 400 1000) (sli t Mv)
                  , Contour mgreens (Sampled 0 0.03 0.7)   (sli t Hp)
                  ]
          | t <- [0,5..195] ]

hydrogenHplus =
  let sli t s = Use (From (Range 0 599) (Range 0 247) (Single 124) t s)
  in Anim [  Draw [ Slice mgreens (sli t Hp)
           --     , Contour mblues  (Sampled 200 400 1000) (sli t Mv)
                  , Contour reds   (Sampled 0 0.04 0.4)   (sli t H2xD)
                  ]
          | t <- [10,20..190] ]

hydrogenHplusStatic =
  let sli s = Use (From (Range 0 599) (Range 0 247) (Single 124) 160 s)
  in Draw [ Slice mgreens (sli Hp)
          , Contour reds   (Sampled 0 0.03 0.4)   (sli H2xD)
          ]

overDensity =
  let sli t s = Use (From (Range 0 599) (Range 0 247) (Single 124) t s)
  in Anim [  Draw [ Slice mblues (sli t D)
                  , Contour mgreens (Sampled 200 400 1000) (sli t Mv)
                  , Contour mreds   (Sampled 0 0.02 0.4)   (sli t H2xD)
                  ]
          | t <- [5,10..195] ]

newFingers =
  let sli t s = Use (From (Range 0 599) (Range 0 247) (Single 124) t s)
  in Anim [  Draw [ Slice mgreens (sli t Hp)
                  , Contour blues (Sampled 1500 500 5000) (sli t G)
                  ]
          | t <- [1..20] ]
-}
