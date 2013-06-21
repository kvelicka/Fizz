{-# LANGUAGE MultiParamTypeClasses #-}

module RawData where

import Data.Word
import Dataset
import RectGrid
import qualified Data.ByteString as Fast


-- Read a grid from a RAW disk file; only the samples are stored on the file.
-- The topology is trivially defined; a function to map points to geometry
-- is defined locally.

data RAWgrid = RAWgrid { raw_xsz :: Int 
                       , raw_ysz :: Int
                       , raw_zsz :: Int 
                       , raw_fnm :: String
                       }

instance Show RAWgrid where
  show g = raw_fnm g


instance Dataset RAWgrid Word8 where
  resource d = raw_fnm d
  read_data (RAWgrid xsz ysz zsz fnm)
    = do { bs <- Fast.readFile $ fnm ++ ".raw"
         ; let vals = {-map (fromRational.toRational) $-} Fast.unpack bs
         ; let dimx = Range 0 (xsz-1)
         ; let dimy = Range 0 (ysz-1)
         ; let dimz = Range 0 (zsz-1)
         ; return $ Grid fnm "<none>" (Dim3D dimx dimy dimz) 0 (minimum vals) (maximum vals) vals
         }
{-
instance Dataset RAWgrid Word8 where
  resource d = raw_fnm d
  read_data (RAWgrid xsz ysz zsz fnm)
    = do { bs <- Fast.readFile $ fnm ++ ".raw"
         ; let vals = Fast.unpack bs
         ; let dimx = Range 0 (xsz-1)
         ; let dimy = Range 0 (ysz-1)
         ; let dimz = Range 0 (zsz-1)
         ; return $ Grid fnm "<none>" (Dim3D dimx dimy dimz) 0 (minimum vals) (maximum vals) vals
         }
-}
{-
readRAWb :: FilePath -> (Int,Int,Int) -> IO ([Word8])
readRAWb fname (isz,jsz,ksz) =
  do{ ima <- Fast.readFile fname
    ; return $ (Fast.unpack ima)
    }

readRAW :: FilePath -> (Int,Int,Int) -> IO ([Float])
readRAW fname (isz,jsz,ksz) =
  do{ ima <- Fast.readFile fname
    ; return $ map (fromRational.toRational) $ (Fast.unpack ima)
    }
-}
