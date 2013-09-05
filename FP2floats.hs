import Data.Binary.Put
import Data.Binary.IEEE754
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as S

import Data.Array.Repa hiding ( (++), map )
import Data.Array.Repa.IO.Binary
import Data.Array.Repa.Repr.ForeignPtr
import System.IO

import FixedPrecisionMath


stringify :: Float -> S.ByteString
stringify a = runPut $ putFloat32le a

main = mainWrite

mainWrite = do
  h <- openBinaryFile ("bytestring.dat") ReadMode 
  b <- BS.hGetContents h
  let floats =  bytesToFloats b
      string = map stringify floats
  --mapM (putStrLn . show)  string
  S.writeFile "floats2.dat" $ S.concat string

mainRead = do
  vs <- readArrayFromStorableFile ("floats2.dat") (Z :. 576600) :: IO (Array F DIM1 Float)
  let floats = toList vs                               
  mapM_ (putStrLn . show) floats
