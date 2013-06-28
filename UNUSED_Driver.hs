{-# LANGUAGE PatternSignatures #-}

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

import qualified Graphics.UI.GLUT as GLUT
import Data.List (nubBy)

import Dataset
import PictureDSL
import Render
import Graphics
import Maths

import RawData

-- TOP LEVEL: execute an expression specifying a picture.
-- The picture expression is first traversed to determine 
-- the files required; access to these via ByteStrings or
-- streams of Floats are generated via "read_data" and 
-- returned as a "context" - a file environment - that is
-- then used by the picture evaluator.  This two-stage
-- process means that the interpreter does not need to 
-- make IO calls itself, simplifying the presentation.

-- run this using GHCi
exec :: (Enum v, Interp v, InvInterp v) => Picture v -> IO()
exec p = do { GLUT.initialize "Astro Viewer" [] >> return ()
            ; let resources = file_list p -- nubBy (\a b -> (fst a) == (fst b)) $ file_list p
            ; context <- mapM snd resources
            ; g <- openGraphics "Scene Viewer" (1000,800)
            ; addScene g $ [eval_picture context p]

{-            ; addScene g $ [Imposter (Group static [axes 600.0 248.0 248.0, eval_picture context p]) 
                                     (bbox 600 248 248) ] -}
            ; GLUT.mainLoop
            }


-- main: if compiling, you must come up with a Picture expression here
main :: IO ()
main = do { exec $ Surface red (Single 10) (Use (RAWgrid 128 128 128 "/home/djd/polyvis/data/hydrogenAtom"))
          ; return ()
          }


