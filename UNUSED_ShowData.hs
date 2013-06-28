import System
import System.IO hiding (hGetContents)
import Data.ByteString.Lazy
import RectGrid
import Sample

main = do
  [filename] <- getArgs
  h <- openFile filename ReadMode
  bs <- hGetContents h
  mapM_ (print.fromSample) $ bytesToSamples bs

