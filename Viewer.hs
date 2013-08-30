{-# LANGUAGE NoMonomorphismRestriction #-}
{- Viewer.hs: a domain-specific language and opengl viewer for
   multi-variate visualization.

   Authors: Rita Borgo & David Duke, University of Leeds, UK
            Colin Runciman & Malcolm Wallace, University of York UK

   Contact: djd@comp.leeds.ac.uk

   Usage: this code is used as a stand-alone program. You need to set
   up the "main" function toward the end of the file to generate the 
   desired picture, then  compile using ghc, and run the executable.

   A number of example visualizations have been pre-defined at the end
   of this file as simple expressions, each of which demonstrates
   the capabilities of the visualizer.

   See the README file and PictureDSL.hs for further details of the picture language.
-}

module Main where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT

import AstroData
import Colour
import Dataset
import Graphics
import Maths
import PictureDSL
import Render

-- TODO change this
-- TOP LEVEL: execute an expression specifying a picture.
-- The picture expression is first traversed to determine 
-- the files required; access to these via ByteStrings or
-- streams of Floats are generated via "read_data" and 
-- returned as a "context" - a file environment - that is
-- then used by the picture evaluator.  This two-stage
-- process means that the interpreter does not need to 
-- make IO calls itself, simplifying the presentation.

evalView :: (Dataset d) => View d Float -> IO()
evalView view@(source :> picture)  = 
  do { GLUT.initialize "Astro Viewer" [] >> return ()
     ; g <- openGraphics "Scene Viewer" (1000,800)
     ; picture <- evalPicture view
     ; addScene g $ [Imposter (Group static [axes 600.0 248.0 248.0, picture]) (bbox 600 248 248) ]
     ; GLUT.mainLoop
     }

main :: IO ()
main = do { evalView $ surface }

{- The remainder of this file contains examples of picture-generating 
   expressions.  These can either be entered into the ghc command line,
   or inserted into the "main" function as above and then compiled.
-}

surface = (from4 35 G) :> 
          (Surface red (Single 2500))

surfaceFull = (fromFull 60 G) :> 
          (Surface red (Single 2500))          

draw = (from4 35 G) :>
       Draw [ (Surface red (Single 2500))
            , (Surface blue (Single 10000))
            , (Surface green (Single 15000))
            ]

surfaceAnim = (from4 35 G) :>
              Anim [Surface green (Single t) | t <- [0, 3000 .. 20000]]

anim = (from4 35 G) :>
       Anim [ (Surface red (Single 2500))
            , (Surface blue (Single 10000))
            , (Surface green (Single 15000))
            ]

volume = (from4 35 G) :>
         (Volume vis5D)

sliced = (VisData (Range 0 599) (Range 0 247) (Single 124) 15 G) :>
         (Slice reds)

contour = (VisData (Range 0 599) (Range 0 247) (Single 124) 15 G) :> 
          (Contour greens (Sampled 1 500 10001))

contourAnim = (VisData (Range 0 599) (Range 0 247) (Single 124) 15 G) :>
              Anim [Contour green (Single t) | t <- [0, 500 .. 20000]]
