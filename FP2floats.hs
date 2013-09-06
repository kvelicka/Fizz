{-
  Small tool for converting old FP (FixedPrecision) files to a binary IEEE
   format float files. Old file is retained, with an -old prefix
  
  Usage:
  ./FP2floats [all|w|rr|rbs] infile 
  w - reads FP-formatted infile, writes float-formatted outfile
  all - specify directory as infile, converts all .dat files to float format
  rr - prints contents of a repa (float-formatted) file to stdout
  rbs - prints contents of a bytestring (FP-formatted) file to stdout
-}
import Control.Monad (zipWithM, zipWithM_)
import Data.Array.Repa hiding ( (++), map )
import Data.Array.Repa.IO.Binary (readArrayFromStorableFile)
import Data.Array.Repa.Repr.ForeignPtr
import Data.Binary.IEEE754 (putFloat32le)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as S
import System.Environment (getArgs)
import System.IO (openBinaryFile, IOMode(ReadMode), hFileSize, hClose)
import System.Posix.Files (rename)
import System.Process (readProcess)
import Text.Regex.Posix

import FixedPrecisionMath

toIEEE :: Float -> S.ByteString
toIEEE a = runPut $ putFloat32le a

-- Gets the list of *.dat files in a directory
files :: String -> IO [String]
files dir = do
  ls <- readProcess ("ls") [dir] []
  let files = lines ls
      pat = "(\\.dat)"
  return $ map ((dir++"/")++) $ filter (\x -> x =~ pat :: Bool) files

-- Renames all files in the list by adding a suffix to their names
-- and returns the list of new names
renameAll :: [String] -> String -> IO [String]
renameAll files suffix = do
  let suffixes = map (++suffix) files
  zipWithM_ rename files suffixes
  return suffixes


write :: String -> String -> IO ()
write input output = do
  h <- openBinaryFile input ReadMode 
  b <- BS.hGetContents h
  let strings = map toIEEE $ bytesToFloats b
  S.writeFile output $ S.concat strings

writeOne :: String -> IO ()
writeOne input = do
  let oldName = input ++ ".dat"
  rename input oldName
  write oldName input

writeAll :: String -> IO ()
writeAll dir = do
  newNames <- files dir
  oldNames <- renameAll newNames "-old"
  zipWithM_ write oldNames newNames

readRepa :: String -> IO ()
readRepa input = do
  -- Get file size to find the required repa array size
  h <- openBinaryFile input ReadMode 
  sz <- hFileSize h
  hClose h
  vs <- readArrayFromStorableFile 
        input
         (Z :. ((fromIntegral sz) `div` 4)) :: IO (Array F DIM1 Float)
  let floats = toList vs                               
  mapM_ (putStrLn . show) floats

readBS :: String -> IO ()
readBS input = do
  h <- openBinaryFile input ReadMode 
  b <- BS.hGetContents h
  let floats =  bytesToFloats b
  mapM_ (putStrLn . show) floats

main = do
  [mode, infile] <- getArgs
  case mode of
    "w"  -> writeOne infile
    "all" -> writeAll infile
    "rr"  -> readRepa infile
    "rbs" -> readBS infile
    _        -> putStrLn "wrong arguments"
