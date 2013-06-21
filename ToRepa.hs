{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import AstroData
import Data.Array.Parallel.Unlifted
import Data.Array.Repa
import System.IO
import Dataset
import System.Environment (getArgs)


main = do [name] <- getArgs
          grid :: Grid3D Double <- read_astro_file name
          let arr = toUArray (values grid)
          let fnm = "repa_"++name++".dat"
          putStrLn $ "Attempting to write "++fnm
          h <- openFile fnm WriteMode
          hPut h arr
          putStrLn "Done."
          