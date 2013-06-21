
module Main where

import V5D
import System.Environment (getArgs)


main = do { [nm] <- getArgs
          ; putStrLn $ "Running on " ++ nm
					; (file, (i,j,k)) <- readV5Dfile nm
					; let fields = map name $ grids file
					; let times  = nr_times file
					; mapM_ (outgrid file) [0 .. times-1]
					}

outgrid file t
		= do { putStrLn $ "<?xml version=\"1.0\"?>"
				 ; putStrLn $ "<VTKFile type=\"ImageData\" version=\"0.1\" byte_order=\"LittleEndian\">"
  			 ; putStrLn $ "<ImageData WholeExtent=\"0 114 0 100 0 74\" Origin=\"0 0 0\" Spacing=\"1 1 1\">"
				 ; putStrLn $ "  <Piece Extent=\"0 114 0 100 0 74\">"
				 ; putStrLn $ "    <PointData>"
         ; mapM_ outfield [((name gr), fst $ dataset file (name gr) t) | gr <- grids file]
				 ; putStrLn $ "    </PointData>"
				 ; putStrLn $ "</ImageData>"
				 ; putStrLn $ "</VTKFile>"
				 }
					
outfield :: (String, [Float]) -> IO ()
outfield (name, vals)
		= do { putStr $ "<DataArray type=\"Float\" Name=\"" 
		     ; putStr $ name
		     ; putStrLn "\" format=\"ascii\">"
				 ; mapM_ outvals $ split 8 vals 
		     ; putStrLn "</DataArray>"
		     }
		  where split _ [] = []
		        split n vs = let (us, ws) = splitAt n vs in us : split n ws

outvals vs = mapM_ (\v -> putStr (show v) >> putStr " ") vs >> putStrLn ""