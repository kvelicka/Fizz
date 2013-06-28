module Main where

import System
import System.IO
import System.Directory
import Data.List
import Data.Maybe
import Control.Monad
import AstroData

main = do
  args <- getArgs
  when (length args /= 2)
       (do prog <- getProgName
           putStrLn ("Usage: "++prog++" dir spec    (spec = desired file)")
           exitWith ExitSuccess )
  let [dir,desired] = args
  okdir <- doesDirectoryExist dir
  when (not okdir)
       (do putStrLn ("directory does not exist: "++dir)
           exitWith ExitSuccess ) 
  existingFiles <- fmap (filter isDotDat) $ getDirectoryContents dir
  let makeThis = either error id $ readSlice desired
  putStrLn ("Planning to make\n\t"++showSlice makeThis)
  let candidateSlices = filter (\slice-> t slice == t makeThis)
                               (mapRight readSlice existingFiles)
  putStrLn ("Candidate starting slices are "
           ++concatMap (("\n\t"++).showSlice) candidateSlices)
  let choice = fromJust (leastDominator candidateSlices makeThis)
  putStrLn ("Least dominator is\n\t"++showSlice choice)
  let cprogargs = sliceToArgs dir choice makeThis
  case sliceToArgs dir choice makeThis of
    Nothing -> do putStrLn ("No work to do")
                  hFlush stdout
    Just cprogargs -> do
      putStrLn ("C program arguments are\n\t"++show cprogargs)
      putStr   ("working... ")
      hFlush stdout
      system ("downsampleC "++showArgs cprogargs)
      putStrLn ("Done.")
      hFlush stdout

isDotDat = isPrefixOf "tad." . reverse
mapRight f   []   = []
mapRight f (x:xs) = case f x of Left _  ->    mapRight f xs;
                                Right a -> a: mapRight f xs;


-- Slice domination --------------------------------------------------------
dominates :: Slice -> Slice -> Bool
slice0 `dominates` slice1 =
       x slice0 `encloses` x slice1
    && y slice0 `encloses` y slice1
    && z slice0 `encloses` z slice1
    && t slice0 `encloses` t slice1
    && all (`from` (s slice0)) (s slice1)

from :: Species -> [Species] -> Bool
S    `from` ss   = any (`elem`ss) [G,S]
R    `from` ss   = any (`elem`ss) [G,R]
H2xD `from` ss   = all (`elem`ss) [H2,D] || H2xD`elem`ss
Mv   `from` ss   = all (`elem`ss) [Cx,Cy,Cz] || Mv`elem`ss
s0   `from` ss   = s0`elem`ss

encloses :: Range Int -> Range Int -> Bool
(Single i)      `encloses` (Single j)      =  i==j
(Range i j)     `encloses` (Single k)      =  i<=k && k<=j
(Range i j)     `encloses` (Range k l)     =  i<=k && l<=j
(Range i j)     `encloses` (Sampled k m n) =  i<=k && n<=j
(Sampled i j k) `encloses` (Single l)      =  l `elem` [i,(i+j)..k]
(Sampled i j k) `encloses` (Sampled l m n) =  l `elem` [i,(i+j)..k]
                                              && n `elem` [i,(i+j)..k]
                                              && m`mod`j == 0
_               `encloses` _               =  False

leastDominator :: [Slice] -> Slice -> Maybe Slice
leastDominator candidates slice =
    case length dominators of
      0 -> Nothing
      1 -> Just (head dominators)
      n -> case least dominators of
             []  -> error "no least dominator"
             [x] -> Just x
             xs  -> error ("multiple least dominators "++ show xs)
  where
    dominators = filter (`dominates`slice) candidates
    least doms = [ d | d <- doms, all (\c-> not $ d`dominates`c)  (doms\\[d]) ]

-- C program interface ------------------------------------------------------
data CprogArgs =
    Args FilePath	-- output filename
         FilePath	-- T dimension: choose a file
         (Int,Int,Int,Int)	-- Z dim: drop init, modulus size, stop, final
         (Int,Int,Int,Int)	-- Y dim: drop init, modulus size, stop, final
         (Int,Int,Int,Int)	-- X dim: drop init, modulus size, stop, final
         (Int,[Derivation])	-- species: how many in input, which to keep
  deriving Show
data Derivation =
    Keep Int
  | Radiation Int
  | Shock Int
  | AbsoluteH2 Int Int
  | MagVort Int Int Int

instance Show Derivation where
  show (Keep i)      = "k "++show i
  show (Radiation i) = "r "++show i
  show (Shock i)     = "s "++show i
  show (AbsoluteH2 h2 d) = "a "++show h2++" "++show d
  show (MagVort cx cy cz) = "m "++show cx++" "++show cy++" "++show cz


derivation :: [Species] -> Species -> Derivation
derivation input desired | desired`elem`input
                         = Keep (fromJust $ elemIndex desired input)
derivation input R       = Radiation  (fromJust $ elemIndex G input)
derivation input S       = Shock      (fromJust $ elemIndex G input)
derivation input H2xD    = AbsoluteH2 (fromJust $ elemIndex H2 input)
                                      (fromJust $ elemIndex D input)
derivation input Mv      = MagVort    (fromJust $ elemIndex Cx input)
                                      (fromJust $ elemIndex Cy input)
                                      (fromJust $ elemIndex Cz input)

sliceToArgs :: FilePath -> Slice -> Slice -> Maybe CprogArgs
sliceToArgs dir dominator makeThis
  | dominator == makeThis = Nothing
  | otherwise = Just $
    Args (dir++"/"++showSlice makeThis)
         (dir++"/"++showSlice dominator++".dat")
         (choose z) (choose y) (choose x)
         (length (s dominator), derivors)
  where
    derivors = map (derivation (s dominator)) (s makeThis)
    choose dim =
      case (dim makeThis, dim dominator) of
        (Single i,      Single _)      -> (0,           1,       0,           0)
        (Single i,      Range j k)     -> (i-j,         1,       i-j,       k-j)
        (Single i,      Sampled j k m) -> ((i-j)`div`k, 1,       (i-j)`div`k,
                                                                    (m-j)`div`k)
        (Range i j,     Range k m)     -> (i-k,         1,       j-k,       m-k)
        (Sampled i j k, Range m n)     -> (i-m,         j,       k-m,       n-m)
        (Sampled i j k, Sampled m n o) -> ((i-m)`div`n, j`div`n, (k-m)`div`n,
                                                                    (o-m)`div`n)
        (_,             _)             -> (0,           0,       0,           0)

showArgs :: CprogArgs -> String
showArgs (Args inf outf (zi,zm,ze,zf) (yi,ym,ye,yf) (xi,xm,xe,xf) (ss,derivs)) =
  intercalate " " ([inf,outf]++map show [zi,zm,ze,zf,yi,ym,ye,yf,xi,xm,xe,xf,ss]
                  ++map show derivs++["0"])

