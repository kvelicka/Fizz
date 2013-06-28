
module Main where

import V5D
import System.Environment (getArgs)
import System.IO

main = do { [nm] <- getArgs
					; (file, (i,j,k)) <- readV5Dfile nm
					; let fields = map name $ grids file
					; let times  = nr_times file
					; mapM_ (outgrid nm file) [0 .. times-1]
					}

outgrid base file t
		= do { let g0 = head $ grids file 
         ; let dx = (nr_rows g0) - 1
         ; let dy = (nr_cols g0) - 1
         ; let dz = (nr_levs g0) - 1         
         ; let ext = "0 " ++ (show dx) ++ " 0 " ++ (show dy) ++ " 0 " ++ (show dz)
         ; h <- openFile (base ++ "-" ++ (show t) ++ ".vti") WriteMode
         ; hPutStrLn h $ "<?xml version=\"1.0\"?>"
				 ; hPutStrLn h $ "<VTKFile type=\"ImageData\" version=\"0.1\" byte_order=\"LittleEndian\">"
  			 ; hPutStrLn h $ "<ImageData WholeExtent=\"" ++ ext ++ "\" Origin=\"0 0 0\" Spacing=\"1 1 1\">"
				 ; hPutStrLn h $ "  <Piece Extent=\"" ++ ext ++ "\">"
				 ; hPutStrLn h $ "    <PointData>"
         ; mapM_ (outfield h) [((name gr), fst $ dataset file (name gr) t) | gr <- grids file]
				 ; hPutStrLn h $ "    </PointData>"
				 ; hPutStrLn h $ "  </Piece>"
				 ; hPutStrLn h $ "</ImageData>"
				 ; hPutStrLn h $ "</VTKFile>"
				 ; hClose h
				 }
					
outfield :: Handle -> (String, [Float]) -> IO ()
outfield h (name, vals)
		= do { hPutStr h $ "<DataArray type=\"Float32\" Name=\"" 
		     ; hPutStr h $ name
		     ; hPutStrLn h "\" format=\"ascii\">"
				 ; mapM_ (outvals h) $ split 8 vals 
		     ; hPutStrLn h "</DataArray>"
		     }
		  where split _ [] = []
		        split n vs = let (us, ws) = splitAt n vs in us : split n ws

outvals h vs = mapM_ (\v -> hPutStr h (show v) >> hPutStr h " ") vs >> hPutStrLn h ""