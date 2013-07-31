module CaseTable where

import Data.List (last, (\\), nub, nubBy, transpose, elemIndex, partition
            , tails, intersect, minimumBy, sortBy)
import Data.Maybe (fromJust)
import Control.Monad (MonadPlus(..), guard, when)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(*) `on` f = \x y -> f x * f y

-- Types describing the original cell.
data Edge a     = Edge a a deriving Show
type Face a     = [a]	-- invariant: closed cycle of vertices
data PolyTope a = PolyTope [Face a] [Edge a]	-- edges derived from faces
  deriving Show
type Marking a  = [a]	-- a set, ordering unimportant.

-- Types describing the extracted surface.
type TriVertex a= Edge a	-- vertex of surface == edge of orig cell
type TriEdge a  = Edge (TriVertex a)
type Triangle a = [TriEdge a] 	-- a cycle of three edges
type VertTriangle a = [TriVertex a] -- a cycle of three vertices

-- Edges are undirected.
instance Eq a => Eq (Edge a) where
  Edge a b == Edge c d  = (a==c&&b==d) || (a==d&&b==c)

-- Vertex representation can be replaced by any other representation.
instance Functor Edge where
  fmap f (Edge a b) = Edge (f a) (f b)
instance Functor PolyTope where
  fmap f (PolyTope faces edges) = PolyTope (map (fmap f) faces)
                                           (map (fmap f) edges)

-- Convert a collection of Face descriptions to a full PolyTope representation,
-- by calculating all the exterior edges.
mkPolyTope :: Eq a => [Face a] -> PolyTope a
mkPolyTope faces = PolyTope faces (edgesFromFaces faces)
  where
    edgesFromFaces = nub . concatMap (\ vs-> allPairs (head vs) vs)
    allPairs a (x:y:zs) = Edge x y: allPairs a (y:zs)
    allPairs a [z]      = Edge z a: []

-- Starting from the reduced cell graph containing only edges through
-- which the surface passes, a Triangle is formed by taking any three
-- cell edges as the TriVertices, thus forming a closed cycle of three
-- TriEdges.
allPossTriangles :: Eq a => PolyTope a -> [Triangle a]
allPossTriangles g@(PolyTope faces verts) =
    uncurry (++) $
    partition (all (isBorder faces)) $	-- re-order for performance only
    nubBy triEq [ result
                | e0 <- verts
                , e1 <- verts \\ [e0]
                , e2 <- verts \\ [e0,e1]
                , let result = [Edge e0 e1, Edge e1 e2, Edge e2 e0]
                  {- if any edge is on a face, it must bound either a
                     marked or unmarked region -}
                , all (validTriEdge g) result
                  -- not all on same face
                , not (flip any faces (\f-> all (triEdgeInFace f) result))
                ]
        -- eliminate rotational isomorphisms, i.e. abc == bca == acb etc
  where triEq t0 t1 = all (`elem` t0) t1

allPossTriEdges :: Eq a => PolyTope a -> [TriEdge a]
allPossTriEdges g@(PolyTope _ verts) =
    nub [ Edge e0 e1 | e0 <- verts
                     , e1 <- verts\\[e0]
                     , validTriEdge g (Edge e0 e1) ]

-- Predicates on TriEdges.
triEdgeInFace :: Eq a => Face a -> TriEdge a -> Bool
triEdgeInFace f (Edge (Edge e0 e1) (Edge e2 e3)) = all (`elem`f) [e0,e1,e2,e3]

edgeInTriEdge :: Eq a => Edge a -> TriEdge a -> Bool
edgeInTriEdge e (Edge a b) = (e==a) || (e==b)

-- For a TriEdge from X to Y, where X and Y are Edges in the same face,
-- there must exist one path of connected edges in the face (of the
-- two possible paths) between X and Y, which traverses only marked
-- or only unmarked vertices.
-- This excludes TriEdges that would force crossings, and therefore
-- by extension excludes intersecting surfaces.
validTriEdge :: Eq a => PolyTope a -> TriEdge a -> Bool
validTriEdge (PolyTope faces incident) te@(Edge a@(Edge e0 e1) b@(Edge e2 e3)) =
    let face = foldr (\f fs-> if triEdgeInFace f te
                              then Just f else fs) Nothing faces
    in case face of
         Nothing -> True
         Just f  -> let edgePath = faceEdges (cycle f) in
                    (e0==e2) || (e0==e3) || (e1==e2) || (e1==e3)
                    || null (intersect incident (pathFrom a b edgePath))
                    || null (intersect incident (pathFrom b a edgePath))
  where
    pathFrom :: Eq a => Edge a -> Edge a -> [Edge a] -> [Edge a]
    pathFrom a b = tail . takeWhile (/=b) . dropWhile (/=a)

    -- Turn an infinite (cyclic) list of vertices describing a face,
    -- into an infinite list of edges describing the same face.
    faceEdges :: [a] -> [Edge a]
    faceEdges (x:y:vertices) = Edge x y: faceEdges (y:vertices)

-- When we choose a triangle with a facial edge, we are disallowed from
-- choosing any other triangle with another edge in the same face that
-- directly adjoins (connects to) the facial edge in the first.
rmAdjacent :: Eq a => PolyTope a -> Triangle a -> [Triangle a] -> [Triangle a]
rmAdjacent g@(PolyTope faces _) tri triset =
    foldr (\ te@(Edge a b) triset->
             let face = foldr (\f fs-> if triEdgeInFace f te
                                       then Just f else fs) Nothing faces
             in case face of
                  Nothing -> triset
                  Just f  -> rmAnyTriInFace f a $
                             rmAnyTriInFace f b $ triset
          )
          triset
          tri
  where
    rmAnyTriInFace f e triset =
      filter (not . any (\te-> triEdgeInFace f te && edgeInTriEdge e te))
             triset

-- A border edge is one that lies on a face of the cell.
isBorder :: Eq a => [Face a] -> TriEdge a -> Bool
isBorder []           triedge   = False
isBorder (face:faces) e@(Edge (Edge a b) (Edge c d))
                                = all (`elem` face) [a,b,c,d]
                                  || isBorder faces e

cellTable :: Eq a => PolyTope a -> [(Marking a, [Triangle a])]
cellTable g = [(m, cellCase g m) | m <- markings g]

cellCase :: Eq a => PolyTope a -> Marking a -> [Triangle a]
cellCase g@(PolyTope faces edges) m
    | minsize==0 = []
    | otherwise  = (head . generatorWithState minsize . incident g) m
  where
    minsize = min (length m) (length (vertices g) - length m)

cellTableVerts :: Eq a => PolyTope a -> [(Marking a, [VertTriangle a])]
cellTableVerts g = [(m, cellCaseVerts g m) | m <- markings g]

cellCaseVerts :: Eq a => PolyTope a -> Marking a -> [VertTriangle a]
cellCaseVerts g m = map (nub . concatMap verts) (cellCase g m)
  where
    verts (Edge a b) = [a,b]

cellCaseSize :: Eq a => PolyTope a -> Marking a -> (Int,[Int])
cellCaseSize g@(PolyTope faces edges) m
    | minsize==0 = (0,[])
    | otherwise  = (minsize, map length
                                 (generatorWithState minsize (incident g m)) )
  where
    minsize = min (length m) (length (vertices g) - length m)

{-
cellCase g@(PolyTope faces edges) m
    | null m || length m == length (vertices g) = []
    | otherwise = let start = incident g m
                  in head (concatMap (\n-> generatorWithState n start) [1..])
-}

markings :: Eq a => PolyTope a -> [Marking a]
markings g = powerset (vertices g) 

vertices :: Eq a => PolyTope a -> [a]
vertices (PolyTope _ edges) = nub $ concat [[v1,v2] | (Edge v1 v2) <- edges]

powerset :: [a] -> [[a]]
powerset = foldr (\x s -> s ++ map (x:) s) [[]]

----

-- A reduced graph containing only cell-edges through which the surface passes
incident :: Eq a => PolyTope a -> Marking a -> PolyTope a
incident g@(PolyTope faces edges) marked =
    PolyTope faces (reduce edges)
  where
--  reduce es = [ Edge a b | a <- marked, b <- unmarked, Edge a b `elem` es ]
    reduce es = ( es \\ [ Edge a b | a <- marked,   b <- marked ]   )
                     \\ [ Edge a b | a <- unmarked, b <- unmarked ]
    unmarked = vertices g \\ marked


----

{-
In order to be a valid surface through the cell,
a set of triangles must satisfy the following conditions:
  * All vertices of the shape are used.
  * All potential border-edges are used zero or once.
  * All potential interior-edges are used zero or twice.

To generate candidate triangle sets, keep a quintuple:
    T   = unused triangles so far (may be used up to once each)
    V   = unused vertices of the figure (must all be used at least once)
    Ef  = unused border/face edges (may be used up to once each)
    Ei2 = completely unused interior edges (may be used up to twice more)
    Ei1 = interior edges already used once (must be used once more)

Aim is to move through a state-space towards positions where
    T   = 0	all triangles have been used (or are no longer possible)
    V   = 0	all vertices have been used
    Ef  = _	don't care, but depleted by at least #V (original V)
    Ei2 = _	don't care, but depleted
    Ei1 = 0	no dangling interior edges

A move through the state space is the choice of a triangle t removed from T
such that:
    * the edges of t are depleted from Ef, Ei2, and Ei1 as follows:
        * any edge of t in Ef  is removed from Ef
        * any edge of t in Ei1 is removed from Ei1
        * any edge of t in Ei2 is removed from Ei2 and added to Ei1
    * any vertex in t is removed from V
    * any triangle t' still in T that can no longer be completely formed
      by edges in Ef, Ei2, and Ei1, must be deleted from T
-}

-- | Running state for generation.
-- Removing an item from one set often requires the removal/addition of
-- items from other sets.  These invariants are implemented in the
-- state-transforming functions, which should be checked for accuracy.
data State a = State
    { setT   :: [Triangle a]	-- unused triangles
    , setV   :: [TriVertex a]	-- unused vertices
    , setEf  :: [TriEdge a]	-- unused face-edges
    , setEi2 :: [TriEdge a]	-- unused interior-edges
    , setEi1 :: [TriEdge a]	-- used-once interior-edges
    , tope   :: PolyTope a	-- read-only, description of the cell
    } deriving Show

rmVerts :: Eq a => [TriVertex a] ->  TriEdge a -> [TriVertex a]
rmVerts verts (Edge a b) = verts\\[a,b]

-- 'complete' is semi-termination: a valid solution exists in this state.
complete :: State a -> Bool
complete s = null (setV s) && null (setEi1 s)

-- a state transformer computing all possible outcomes
data Generator a b = Gen (State a -> [(b, State a)])
instance Functor (Generator a) where
    fmap f (Gen g) = Gen (map (\ (v,s') -> (f v, s')) . g)
instance Monad (Generator a) where
    return v       = Gen (\s-> [(v,s)])
    (Gen g) >>= f  = Gen ( concat
                         . map (\ (v,s') -> case f v of (Gen f') -> f' s')
                      -- . uncurry (++)
                      -- . partition (complete . snd)
                         . g)
instance MonadPlus (Generator a) where
    mzero = Gen (\s-> [])
    mplus (Gen g) (Gen h) = Gen (\s-> g s ++ h s)

-- the generator
generatorWithState :: Eq a => Int -> PolyTope a -> [[Triangle a]]
generatorWithState n g = applyGen (triSet n []) (initState g)

-- initial state, based on cell description
initState g@(PolyTope faces verts) =
                State { setT   = allPossTriangles g
                      , setV   = verts
                      , setEf  = filter (isBorder faces) edges
                      , setEi2 = filter (not . isBorder faces) edges 
                      , setEi1 = []
                      , tope   = g
                      }
  where
    edges = allPossTriEdges g

-- specific operations for this particular generator monad:

-- * Apply the generator to an initial state
applyGen :: Generator a v -> State a -> [v]
applyGen (Gen f) s = map fst (f s)

-- * Read and write the state
getState     :: Generator a (State a)
getState      = Gen (\s-> [(s,s)])
amendState   :: (State a->State a) -> Generator a ()
amendState f  = Gen (\s-> [((),f s)])

-- * Termination condition (success):
--      We have found one or more surfaces that fully cover the space.
stateTrigger :: Generator a Bool
stateTrigger = fmap (\s-> null (setV s) && null (setEi1 s)) $ getState

-- * Termination condition (failure):
--      Either we have run out of edges, or out of triangles.
finished :: Generator a Bool
finished = fmap (\s-> null (setT s) || null (setEf s++setEi2 s++setEi1 s))
                getState

-- branch the search tree based on a choice from a portion of the state
choose :: (State a->[b]) -> (b->State a->State a) -> Generator a b
choose project transform = Gen (\s-> map (\v-> (v, transform v s))
                                         (project s) )

-- specialise 'choose' over setT, and diagonalise to eliminate duplicates
chooseDiag :: (Triangle a->State a->State a) -> Generator a (Triangle a)
chooseDiag transform = Gen (\s-> map (\ (t:ts)-> (t, transform t s{setT=ts}))
                                     (init (tails (setT s))) )


-- Find all trisets up to given size.
triSet :: Eq a => Int -> [Triangle a] -> Generator a [Triangle a]
triSet n already = do
    guard =<< return (n>0)
    guard =<< fmap not finished
    tri  <- chooseTriangle
    singleSurface <- fmap (null. setEi1) getState
    when singleSurface (amendState (rmConnecting (tri:already)))
    emit <- stateTrigger
    (if emit then return else triSet (n-1)) (tri:already)

-- The main state-transition function.
chooseTriangle :: Eq a => Generator a (Triangle a)
chooseTriangle = {-choose setT deplete-} chooseDiag deplete
  where
    deplete tri s = let s' = rmTriangle tri s
                        ts = setT s'
                    in s' { setT = rmAdjacent (tope  s') tri
                                   $ filter (permissible s')
                                   $ ts }
    rmTriangle tri s = s { setT   = setT s\\[tri]
                         , setV   = filter (`notElem` vertsIn tri) (setV s)
                         , setEf  = filter (`notElem` tri) (setEf s)
                         , setEi1 = filter (`notElem` tri) (setEi1 s)
                                    ++ filter (`elem` tri) (setEi2 s)
                         , setEi2 = filter (`notElem` tri) (setEi2 s)
                         }
    permissible s tri = all (`elem` (setEf s++setEi2 s++setEi1 s)) tri

vertsIn :: Triangle a -> [TriVertex a]
vertsIn = concatMap (\ (Edge a b) -> [a,b])

-- To avoid disjoint but connected surfaces.  Multiple surfaces are
-- permitted, but only if they do not touch.
rmConnecting :: Eq a => [Triangle a] -> State a -> State a
rmConnecting ts s = s { setT = filter (not . vertsInTris ts) (setT s) }
  where vertsInTris ts = let vs = concatMap vertsIn ts in
                         any (`elem` vs) . vertsIn
