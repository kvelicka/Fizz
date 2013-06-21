{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeOperators, EmptyDataDecls,
             ScopedTypeVariables, BangPatterns, TypeSynonymInstances,TypeOperators, 
             FunctionalDependencies, ExistentialQuantification,ForeignFunctionInterface #-}

module Main where

import Prelude hiding (lookup)

import Graphics.Rendering.OpenGL.GL(Vertex2(..), Vertex3(..))
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Array.Parallel.Unlifted as U
import qualified Data.Array.Parallel.Stream as Str

import Control.Applicative

import Text.ParserCombinators.Poly

import System.IO.Unsafe (unsafePerformIO)
import System.IO
import System.Posix.Files
import GHC.Int
import Numeric

import Data.Bits
import Data.Char
import Data.Array
import Data.List ((!!), elemIndex,zipWith4, nub)
import Data.Maybe
import Data.Word
import Data.Ratio
import qualified Data.ByteString.Lazy as Fast

import Colour
import Render
import Graphics
import Sample hiding (s)
import FixedPrecision
import CaseTable

-- Dataset -------------------------------

data Z = Z
data a :. b = a :. b

class Dim d where
  size :: d -> Int
  dims :: d -> [Int]
  
instance Dim Z where
  size = const 0
  dims = const []
  
instance Dim a => Dim (a :. Int) where
  size (a :. b) = size a * b
  dims (a :. b) = dims a ++ [b]
  
type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. Int
type DIM3 = DIM2 :. Int

data Grid sh v = Grid { origin  :: String
                      , field   :: String
                      , shape   :: sh
                      , time    :: Int
                      , minv    :: v
                      , maxv    :: v
                      , values  :: Fast.ByteString
                      } deriving Show

type Grid3D a = Grid DIM3 a
type Grid2D a = Grid DIM2 a


dim_x, dim_y, dim_z :: Dim d => d -> Int
dim_x d = let [_,_,x] = dims d in x
dim_y d = let [_,y,_] = dims d in y 
dim_z d = let [z,_,_] = dims d in z

-- AstroData ------------------------------------

read_astro :: DIM3 -> String -> IO [Float]
read_astro dim basename
    = do let (Z :. zr :. yr :. xr) = dim
         let size = zr * yr * xr
         h <- openBinaryFile (basename++".dat") ReadMode    
         bs <- Fast.hGetContents h
         return $ bytesToValues bs
{-# INLINE read_astro #-}
  
bytesToSamples :: Fast.ByteString -> [Sample]
bytesToSamples bs
    | Fast.null bs   = []
    | otherwise      = (Sample s):bytesToSamples post
                       where
                           (pre,post) = Fast.splitAt 4 bs
                           [a,b,c,d]  = Fast.unpack pre
                           s = (a32 .|. b32 .|. c32 .|. d32)
                           a32 :: Word32 = fromIntegral a `shiftL` 24
                           b32 :: Word32 = fromIntegral b `shiftL` 16
                           c32 :: Word32 = fromIntegral c `shiftL`  8
                           d32 :: Word32 = fromIntegral d

charsToWord32 :: [Char] -> [Word32]
charsToWord32 [] = []
charsToWord32 (d:c:b:a:rest) = ((fromIntegral (fromEnum a) `shiftL` 24) 
                                .|. (fromIntegral (fromEnum b) `shiftL` 16) 
                                .|. (fromIntegral (fromEnum c) `shiftL`  8) 
                                .|. (fromIntegral (fromEnum d))): charsToWord32 rest

bytesToValues :: Fast.ByteString -> [Float]
bytesToValues bs
    | Fast.null bs   = []
    | otherwise      = (realToFrac val):bytesToValues post
                       where
                           (pre,post) = Fast.splitAt 4 bs
                           [a,b,c,d]  = Fast.unpack pre
                           a16 :: Int16 = fromIntegral a `shiftL` 8
                           b16 :: Int16 = fromIntegral b 
                           c16 :: Int16 = fromIntegral c `shiftL`  8
                           d16 :: Int16 = fromIntegral d    
                           exp = a16 .|. b16
                           man = c16 .|. d16
                           val | exp < 0   = toInteger man % (10 ^ negate (toInteger exp))
                               | otherwise = toInteger man * (10^exp) % 1  
{-# INLINE bytesToValues #-}

-- RectGrid -------------------------------------------------------------------
data Cell_8 a = Cell_8 !a !a !a !a !a !a !a !a deriving (Eq,Ord,Show)

instance  (Ix a) => Ix (Cell_8 a)  where
    range ((Cell_8 l1 l2 l3 l4 l5 l6 l7 l8),(Cell_8 u1 u2 u3 u4 u5 u6 u7 u8)) =
          [(Cell_8 i1 i2 i3 i4 i5 i6 i7 i8) 
                                     | i8 <- range (l8,u8)
                                     , i7 <- range (l7,u7)
                                     , i6 <- range (l6,u6)
                                     , i5 <- range (l5,u5)
                                     , i4 <- range (l4,u4)
                                     , i3 <- range (l3,u3)
                                     , i2 <- range (l2,u2)
                                     , i1 <- range (l1,u1) ]
	-- Note little-endian order of the 8-tuple.

    index ((Cell_8 l1 l2 l3 l4 l5 l6 l7 l8),(Cell_8 u1 u2 u3 u4 u5 u6 u7 u8))
          (Cell_8 i1 i2 i3 i4 i5 i6 i7 i8) =
       index (l1,u1) i1 + rangeSize (l1,u1) * (
        index (l2,u2) i2 + rangeSize (l2,u2) * (
         index (l3,u3) i3 + rangeSize (l3,u3) * (
          index (l4,u4) i4 + rangeSize (l4,u4) * (
           index (l5,u5) i5 + rangeSize (l5,u5) * (
            index (l6,u6) i6 + rangeSize (l6,u6) * (
             index (l7,u7) i7 + rangeSize (l7,u7) * (
              index (l8,u8) i8)))))))

    inRange ((Cell_8 l1 l2 l3 l4 l5 l6 l7 l8),(Cell_8 u1 u2 u3 u4 u5 u6 u7 u8))
            (Cell_8 i1 i2 i3 i4 i5 i6 i7 i8) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
           && inRange (l5,u5) i5
           && inRange (l6,u6) i6
           && inRange (l7,u7) i7
           && inRange (l8,u8) i8

instance Functor Cell_8 where
  fmap f (Cell_8 v1 v2 v3 v4 v5 v6 v7 v8) 
    = Cell_8 (f v1) (f v2) (f v3) (f v4) (f v5) (f v6) (f v7) (f v8)

instance Applicative Cell_8 where
  pure f = Cell_8 f f f f f f f f
  (Cell_8 f1 f2 f3 f4 f5 f6 f7 f8) <*> (Cell_8 v1 v2 v3 v4 v5 v6 v7 v8)
    = Cell_8 (f1 v1) (f2 v2) (f3 v3) (f4 v4) (f5 v5) (f6 v6) (f7 v7) (f8 v8)

instance (Bounded a) => Bounded (Cell_8 a) where
  minBound = let x = minBound in Cell_8 x x x x x x x x
  maxBound = let x = maxBound in Cell_8 x x x x x x x x

instance (Enum a, Bounded a) => Enum (Cell_8 a) where
  toEnum x = let step = 1 + fromEnum (maxBound`asTypeOf`contents)
                 (sa,a) = x  `divMod` step
                 (sb,b) = sa `divMod` step
                 (sc,c) = sb `divMod` step
                 (sd,d) = sc `divMod` step
                 (se,e) = sd `divMod` step
                 (sf,f) = se `divMod` step
                 (sg,g) = sf `divMod` step
                 (sh,h) = sg `divMod` step
                 contents = toEnum a
             in Cell_8 contents   (toEnum b) (toEnum c) (toEnum d)
                       (toEnum e) (toEnum f) (toEnum g) (toEnum h)
  fromEnum (Cell_8 a b c d e f g h) = 
      let step = 1 + fromEnum (maxBound`asTypeOf`a) in
      fromEnum a + step*
       (fromEnum b + step*
        (fromEnum c + step*
         (fromEnum d + step*
          (fromEnum e + step*
           (fromEnum f + step*
            (fromEnum g + step*
              (fromEnum h)))))))

-- Make Cell_8 an instance of Cell; note the order of nodes in the vertices
-- mapping is important, and corresponds to the enumeration of the cell 
-- vertices used by the marching cube table.

zipWith8 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) =
    z a b c d e f g h : zipWith8 z as bs cs ds es fs gs hs
zipWith8 _ _ _ _ _ _ _ _ _ = []
{-# INLINE zipWith8 #-}
                                    
instance Cell Cell_8 MyVertex where
  --data Facet Cell_8 x = FacTri x x x
  select n (Cell_8 a b c d e f g h)
    = case n of
        A  -> a
        B  -> b
        C  -> c
        D  -> d
        E  -> e
        F  -> f
        G  -> g
        H  -> h
  mc_case = let table = array (minBound,maxBound::Cell_8 Bool)
                              (map (\ (a,b)-> ( markingToCellBool a
                                              , concatMap (map edgeToPair) b) )
                                   (cell_table_verts cube))
                markingToCellBool m = let q v = v`elem`m in
                    Cell_8 (q A) (q B) (q C) (q D) (q E) (q F) (q G) (q H)
                edgeToPair (Edge a b) = (a,b)
            in (table!)

-- Generate a dataset consisting of the coordinates in a (xsz x ysz x zsz)-cube.
-- Note that the origin of the cube is (0,0,0), and that the components refer
-- to the number of CELLS along each dimension.
cubicGeom :: (Floating a) => Grid DIM3 v -> [Cell_8 MyVertex (Vertex3 a)]
cubicGeom f 
    = cubicGrid (shape f) $ (cubicPoints f)
{-# INLINE cubicGeom #-}

-- Generate a list of coordinates for a (xsz x ysz x zsz)-cube, starting
-- from (0,0,0).
cubicPoints :: (Num a) => Grid DIM3 v -> [Vertex3 a]
cubicPoints g = [ Vertex3 (fromIntegral i) (fromIntegral j) (fromIntegral k)
                | k <- [0 .. dim_z (shape g) - 1]
                , j <- [0 .. dim_y (shape g) - 1]
                , i <- [0 .. dim_x (shape g) - 1]
                ]
{-# INLINE cubicPoints #-}

-- Generate a stream (dataset) of 8-tuple cell samples taken from
-- an input stream of values.  The dataset is an (xmax x ymax x zmax)
-- cube where the components here refer to the size of a dimension 
-- in POINTs.
cubicGrid :: DIM3 -> [a] -> [Cell_8 MyVertex a]
cubicGrid (Z :. zmax :. ymax :. xmax) 
    = (discontinuities (0,0,0)) . zipCube
      where
          zipCube stream = zipWith8 Cell_8 stream
                                           (drop 1 stream)
                                           (drop (line+1) stream)
                                           (drop line stream)
                                           (drop plane stream)
                                           (drop (plane+1) stream)
                                           (drop (plane+line+1) stream)
                                           (drop (plane+line) stream)
          line  = xmax
          plane = (xmax)*(ymax)

          discontinuities _ [] = []
          discontinuities (i,j,k) (x:xs)
              | k==(zmax-1)   = []
              | j==(ymax-1)   =    discontinuities (0,0,k+1) (drop (xmax-1) xs)
              | i==(xmax-1)   =    discontinuities (0,j+1,k) xs
              | otherwise     = x: discontinuities (i+1,j,k) xs
{-# INLINE cubicGrid #-}

-- Cell Types ---------------------------------------------------------
          
class (Functor c, Enum v) => Cell c v | c -> v where
  select   :: v  -> c a -> a
  {-# INLINE select #-}
  mc_case  :: c Bool -> [(v,v)] --[Facet c (v,v)]


---- test data
data MyVertex = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P
  deriving (Eq,Show,Enum)

icube :: PolyTope Int
icube    = mkPolyTope [ [0,1,2,3], [4,5,6,7], [0,1,4,5]
                      , [2,3,6,7], [0,3,4,7], [1,2,5,6] ]


cube :: PolyTope MyVertex -- 3D
cube     = mkPolyTope [ [A,B,C,D], [E,F,G,H], [A,B,F,E]
                      , [C,D,H,G], [A,D,H,E], [B,C,G,F] ]

-- first arg is scalar
locateCube :: GL.GLfloat -> MyVertex -> GL.Vertex3 GL.GLfloat
locateCube n A = Vertex3  0.0 n   0.0
locateCube n B = Vertex3  n   n   0.0
locateCube n C = Vertex3  n   0.0 0.0
locateCube n D = Vertex3  0.0 0.0 0.0
locateCube n E = Vertex3  0.0 n   n
locateCube n F = Vertex3  n   n   n
locateCube n G = Vertex3  n   0.0 n
locateCube n H = Vertex3  0.0 0.0 n


-- Maths -------------------------------------------------
class Interp b where
  interp :: Float -> b -> b -> b
  {-# INLINE interp #-}

class Real b => InvInterp b where
  inv_interp :: b -> b -> b -> Float
  {-# INLINE inv_interp #-}

instance Interp Float where
  interp t v1 v2 = (1-t)*v1 + t*v2

instance Interp GL.GLfloat where
  interp t v1 v2 = let tf = realToFrac t in (1-tf)*v1 + tf*v2

instance InvInterp Float where
  inv_interp s v1 v2 = (s - v1) / (v2 - v1)

instance (Interp a, Applicative f) => Interp (f a) where
  interp t x y = pure (interp t) <*> x <*> y


-- Algorithms -----------------------------------------

-- Generate the isosurface at a given threshold. Note that the surface is 
-- returned implicitly as a list of points - adjacent groups of three points
-- define the triangles of the surface.
-- (A list of points rather than triangles is used, as it matches better the
-- interface to OpenGL).



-- Picture DSL -----------------------------------------

data Colour = Brewer Palette | Vis5D | X11 [ColourName] deriving Show
red = X11 [Red]

data DataExpr v = Use VisData deriving Show
data Real v => Picture v = Surface Colour v (Grid3D v) 
                         
transfer :: Real v => Colour -> GL.GLfloat -> v -> v -> v -> GL.Color4 GL.GLfloat
transfer Vis5D _     minv maxv 
    = transfer_f (minv, maxv)
transfer (Brewer color) alpha minv maxv 
    = transfer_t (minv, maxv) (brewer color alpha)
transfer (X11 names) alpha minv maxv
    = transfer_t (minv, maxv) (map (x11_rgb alpha) names)


{-# SPECIALISE eval_picture :: Picture Double -> HsScene  #-}
{-# SPECIALISE eval_picture :: Picture Float -> HsScene  #-}
eval_picture :: (Enum a, Interp a, Fractional a, InvInterp a {-, U.Elt a -}) => Picture a -> HsScene
               
eval_picture (Surface pal th field)
    = Group static geomlist
      where
          -- mkgrid = cubicGrid (shape field)         -- ::   [a] -> Cells Cell_8 MyVertex a
          colour = transfer pal 1.0 1.0 1.0
          geomlist = surface_geom (concat $ iso3D th field) (colour 1.0)

exec :: (Enum v, Interp v, Fractional v, InvInterp v {-, U.Elt v -}) => Picture v -> IO()
exec p = do { GLUT.initialize "Astro Viewer" [] >> return ()
            ; g <- openGraphics "Scene Viewer" (1000,800)
            ; addScene g (Vertex3 0 0 0, Vertex3 150 62 62) $ [eval_picture p]
            ; GLUT.mainLoop
            }

-- main: if compiling, you must come up with a Picture expression here
main :: IO ()
main = do { ds :: Grid3D Float {-Double-} <- read_astro (Z :. 62 :. 62 :. 150) "x0-4-599y0-4-247z0-4-247t60.G" 
          ; let pic = Surface red 16000 ds
          ; exec pic
          }
