{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
-- Implementation of a dataset with a regular, rectangular
-- grid as topology.  This is essentially the dataset
-- type used in the Vis'06 paper.

module RectGrid where

import Control.Applicative
import Data.Array
import Data.Array.Repa hiding (index, (!))
import Data.List (zipWith4)
import Graphics.Rendering.OpenGL.GL (Vertex3(..))

import CaseTable
import CellTypes
import Dataset
import McLookupTable

-- Cells in the dataset are cubes.

instance  (Ix a) => Ix (Cell8 a)  where
    range ((Cell8 l1 l2 l3 l4 l5 l6 l7 l8),(Cell8 u1 u2 u3 u4 u5 u6 u7 u8)) =
          [(Cell8 i1 i2 i3 i4 i5 i6 i7 i8) 
                                     | i8 <- range (l8,u8)
                                     , i7 <- range (l7,u7)
                                     , i6 <- range (l6,u6)
                                     , i5 <- range (l5,u5)
                                     , i4 <- range (l4,u4)
                                     , i3 <- range (l3,u3)
                                     , i2 <- range (l2,u2)
                                     , i1 <- range (l1,u1) ]
	-- Note little-endian order of the 8-tuple.

    index ((Cell8 l1 l2 l3 l4 l5 l6 l7 l8),(Cell8 u1 u2 u3 u4 u5 u6 u7 u8))
          (Cell8 i1 i2 i3 i4 i5 i6 i7 i8) =
       let a = index (l1,u1) i1 + rangeSize (l1,u1) * (
                index (l2,u2) i2 + rangeSize (l2,u2) * (
                 index (l3,u3) i3 + rangeSize (l3,u3) * (
                  index (l4,u4) i4 + rangeSize (l4,u4) * (
                   index (l5,u5) i5 + rangeSize (l5,u5) * (
                    index (l6,u6) i6 + rangeSize (l6,u6) * (
                     index (l7,u7) i7 + rangeSize (l7,u7) * (
                      index (l8,u8) i8))))))) in a

    inRange ((Cell8 l1 l2 l3 l4 l5 l6 l7 l8),(Cell8 u1 u2 u3 u4 u5 u6 u7 u8))
            (Cell8 i1 i2 i3 i4 i5 i6 i7 i8) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
           && inRange (l5,u5) i5
           && inRange (l6,u6) i6
           && inRange (l7,u7) i7
           && inRange (l8,u8) i8

instance  (Ix a) => Ix (Cell4 a)  where
    range ((Cell4 l1 l2 l3 l4),(Cell4 u1 u2 u3 u4)) =
          [(Cell4 i1 i2 i3 i4) | i4 <- range (l4,u4)
                                , i3 <- range (l3,u3)
                                , i2 <- range (l2,u2)
                                , i1 <- range (l1,u1) ]
    index ((Cell4 l1 l2 l3 l4),(Cell4 u1 u2 u3 u4))
           (Cell4 i1 i2 i3 i4) =
       index (l1,u1) i1 + rangeSize (l1,u1) * (
        index (l2,u2) i2 + rangeSize (l2,u2) * (
         index (l3,u3) i3 + rangeSize (l3,u3) * (
          index (l4,u4) i4)))

    inRange ((Cell4 l1 l2 l3 l4),(Cell4 u1 u2 u3 u4))
            (Cell4 i1 i2 i3 i4) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4

instance Functor Cell8 where
  fmap f (Cell8 v1 v2 v3 v4 v5 v6 v7 v8) 
    = Cell8 (f v1) (f v2) (f v3) (f v4) (f v5) (f v6) (f v7) (f v8)

instance Functor Cell4 where
  fmap f (Cell4 v1 v2 v3 v4) 
    = Cell4 (f v1) (f v2) (f v3) (f v4)

instance Applicative Cell8 where
  pure f = Cell8 f f f f f f f f
  (Cell8 f1 f2 f3 f4 f5 f6 f7 f8) <*> (Cell8 v1 v2 v3 v4 v5 v6 v7 v8)
    = Cell8 (f1 v1) (f2 v2) (f3 v3) (f4 v4) (f5 v5) (f6 v6) (f7 v7) (f8 v8)

instance Applicative Cell4 where
  pure f = Cell4 f f f f
  (Cell4 f1 f2 f3 f4) <*> (Cell4 v1 v2 v3 v4)
    = Cell4 (f1 v1) (f2 v2) (f3 v3) (f4 v4)

instance (Bounded a) => Bounded (Cell8 a) where
  minBound = let x = minBound in Cell8 x x x x x x x x
  maxBound = let x = maxBound in Cell8 x x x x x x x x

instance (Bounded a) => Bounded (Cell4 a) where
  minBound = let x = minBound in Cell4 x x x x
  maxBound = let x = maxBound in Cell4 x x x x

instance (Enum a, Bounded a) => Enum (Cell8 a) where
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
             in Cell8 contents   (toEnum b) (toEnum c) (toEnum d)
                       (toEnum e) (toEnum f) (toEnum g) (toEnum h)
  fromEnum (Cell8 a b c d e f g h) = 
      let step = 1 + fromEnum (maxBound`asTypeOf`a) in
      fromEnum a + step*
       (fromEnum b + step*
        (fromEnum c + step*
         (fromEnum d + step*
          (fromEnum e + step*
           (fromEnum f + step*
            (fromEnum g + step*
              (fromEnum h)))))))

instance (Enum a, Bounded a) => Enum (Cell4 a) where
  toEnum x = let step = 1 + fromEnum (maxBound`asTypeOf`contents)
                 (sa,a) = x  `divMod` step
                 (sb,b) = sa `divMod` step
                 (sc,c) = sb `divMod` step
                 (sd,d) = sc `divMod` step
                 contents = toEnum a
             in Cell4 contents   (toEnum b) (toEnum c) (toEnum d)
  fromEnum (Cell4 a b c d) = 
      let step = 1 + fromEnum (maxBound`asTypeOf`a) in
      fromEnum a + step*
       (fromEnum b + step*
        (fromEnum c + step*
         (fromEnum d)))


-- Make Cell8 an instance of Cell; note the order of nodes in the vertices
-- mapping is important, and corresponds to the enumeration of the cell 
-- vertices used by the marching cube table.

zipWith8 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) =
    z a b c d e f g h : zipWith8 z as bs cs ds es fs gs hs
zipWith8 _ _ _ _ _ _ _ _ _ = []
                                    
instance Cell Cell8 MyVertex where
  data Facet Cell8 x = FacTri x x x
  select n (Cell8 a b c d e f g h)
    = case n of
        A  -> a
        B  -> b
        C  -> c
        D  -> d
        E  -> e
        F  -> f
        G  -> g
        H  -> h
  mcCase = mcLookup

instance Cell Cell4 MyVertex where
  data Facet Cell4 x = FacLine x x
  select n (Cell4 a b c d)
    = case n of
        A  -> a
        B  -> b
        C  -> c
        D  -> d
  mcCase = let table = listArray (Cell4 False False False False, Cell4 True True True True) $
                        [ {- F, F, F, F -} []
                        , {- T, F, F, F -} [(A,B), (A,D)]
                        , {- F, T, F, F -} [(A,B), (B,C)]
                        , {- T, T, F, F -} [(B,C), (A,D)]
                        , {- F, F, T, F -} [(B,C), (C,D)]
                        , {- T, F, T, F -} [(A,B), (A,D), (B,C), (C,D)]
                        , {- F, T, T, F -} [(A,B), (C,D)]
                        , {- T, T, T, F -} [(D,A), (D,C)]
                        , {- F, F, F, T -} [(D,A), (D,C)]
                        , {- T, F, F, T -} [(A,B), (C,D)]
                        , {- F, T, F, T -} [(A,B), (B,C), (C,D), (D,A)]
                        , {- T, T, F, T -} [(B,C), (C,D)]
                        , {- F, F, T, T -} [(B,C), (A,D)]
                        , {- T, F, T, T -} [(A,B), (B,C)]
                        , {- F, T, T, T -} [(A,D), (A,B)]
                        , {- T, T, T, T -} []
                        ]
            in (table!)

-- Generate a dataset consisting of the coordinates in a (xsz x ysz x zsz)-cube.
-- Note that the origin of the cube is (0,0,0), and that the components refer
-- to the number of CELLS along each dimension.
cubicGeom :: (Floating a) => FizzData v -> Cells Cell8 MyVertex (Vertex3 a)
cubicGeom f 
    = cubicGrid (dimensions $ samplings f) $ (cubicPoints f)

-- Generate a list of coordinates for a (xsz x ysz x zsz)-cube, starting
-- from (0,0,0).
cubicPoints :: (Num a) => FizzData v -> [Vertex3 a]
cubicPoints g = [ Vertex3 (fromIntegral i) (fromIntegral j) (fromIntegral k)
                | k <- listZ (samplings g)
                , j <- listY (samplings g)
                , i <- listX (samplings g)
                ]

squarePoints :: (Num a) => FizzData v -> [Vertex3 a]
squarePoints g = [ Vertex3 (fromIntegral i) (fromIntegral j) 124
                 | j <- listY (samplings g)
                 , i <- listX (samplings g)
                 ]
-- Generate a stream (dataset) of 8-tuple cell samples taken from
-- an input stream of values.  The dataset is an (xmax x ymax x zmax)
-- cube where the components here refer to the size of a dimension 
-- in POINTs.

cubicGrid :: (Int,Int,Int) -> [a] -> Cells Cell8 MyVertex a
cubicGrid (xmax,ymax,zmax) 
    = Cells . (discontinuities (0,0,0)) . zipCube
      where
          zipCube stream = zipWith8 Cell8 stream
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

squareGrid :: (Int, Int) -> [a] -> Cells Cell4 MyVertex a
squareGrid (xmax,ymax) 
    = Cells . (discontinuities (0,0)) . zipSquare
      where
          zipSquare stream = zipWith4 Cell4 stream
                                             (drop 1 stream)
                                             (drop (line+1) stream)
                                             (drop line stream)
          line  = xmax
          discontinuities _ [] = []
          discontinuities (i,j) (x:xs)
              | j==(ymax-1)   = []
              | i==(xmax-1)   =    discontinuities (0,j+1) xs
              | otherwise = x: discontinuities (i+1,j) xs



