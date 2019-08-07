{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-|
Module      : Control.Reduce.Graph
Description : A module for representing graphs
Copyright   : (c) Christian Kalhauge
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

A module for handling graphs to do reduction. This module builds on the
descriptions and algorithms of the `containers` library, and mostly adds
support for edge labels and uses the `vector` library instead of `arrays`.
-}
module Control.Reduce.Graph
  (
  -- * The `Graph`
    Graph (..)
  , empty
  , buildGraph
  , buildGraph'
  , buildGraphFromNodesAndEdges
  , edges
  , nodeLabels

  , Node (..)
  , buildNode
  , outEdges

  , Edge (..)

  , reverseEdges

  -- ** Transformations
  , transposeG

  -- * Algorithms
  , dff
  , dfs
  , scc
  , scc'
  , sccN
  , partition
  , componentMap

  , closures
  , closuresN

  -- * Reading and writing graphs

  , readTGF
  , readCSV
  , readEdgesCSV

  ) where

-- lens
import           Control.Lens

-- base
import           GHC.Generics (Generic)
import           Control.Monad
import           Control.Monad.ST
import           Data.Char
import           Data.Foldable
import qualified Data.List                   as L
import           Data.Maybe                  (catMaybes, fromMaybe)
import           Data.Void
import           Data.Bifunctor

-- import Debug.Trace

-- containers
import qualified Data.IntSet                 as IS
import qualified Data.Set                    as S
import qualified Data.Tree                   as T (Forest, Tree (Node))

-- vector
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- text
import qualified Data.Text.Lazy              as T

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- cassava
import qualified Data.Csv as C

-- megaparsec
import           Text.Megaparsec             hiding (empty)
import           Text.Megaparsec.Char

-- | A `Vertex`
type Vertex = Int

-- | An `Edge`
data Edge e n = Edge !n !n !e
  deriving (Show, Eq, Generic, Functor)

instance Bifunctor Edge where
  first f (Edge a b e) = (Edge a b (f e))
  second = fmap

-- | A `Node` is a label, and a list of edges.
data Node e n = Node
  { nodeLabel    :: !n
  , edgeVertices :: !(U.Vector Vertex)
  , edgeLabels   :: !(V.Vector e)
  } deriving (Show, Eq)

-- | Get the outedges of a node
outEdges :: Node e n -> V.Vector (Vertex, e)
outEdges Node{..} =
  V.zip (G.convert edgeVertices) edgeLabels
{-# INLINE outEdges #-}

buildNode :: n -> [(Vertex, e)] -> Node e n
buildNode n edges' =
  Node n (U.fromList a) (V.fromList b)
  where (a, b) = unzip . L.sortOn fst $ edges'

-- | Graph is a vector of nodes.
newtype Graph e n = Graph
  { nodes          :: V.Vector (Node e n)
  } deriving (Show, Eq)

nodeLabels :: Graph e n -> V.Vector n
nodeLabels = V.map nodeLabel . nodes
{-# INLINE nodeLabels #-}

empty :: Graph e n
empty = Graph (V.empty)
{-# INLINE empty #-}

-- | Build a graph form an adjacency list.
buildGraph :: (Ord key) => [(n, key, [(key, e)])] -> (Graph e n, key -> Maybe Vertex)
buildGraph nodes =
  (graph, lookupKey)
  where
    sortedNodes = V.fromList $ L.sortOn (\(_, k, _) -> k) nodes
    keyMap = V.map (\(_, k, _) -> k) $ sortedNodes
    graph = Graph . V.map (\(n, _, lst) -> buildNode n $ toNodes lst) $ sortedNodes
    toNodes edges' = catMaybes [(,e') <$> lookupKey k' | (k', e') <- edges']
    lookupKey = binarySearch keyMap

buildGraph' :: Ord n => [(n, [n])] -> (Graph () n, n -> Maybe Vertex)
buildGraph' nodes' =
  buildGraph [(n, n, map (,()) edges') | (n, edges') <- nodes' ]

buildGraphFromNodesAndEdges :: (Ord key) => [(key, n)] -> [Edge e key] -> (Graph e n, key -> Maybe Vertex)
buildGraphFromNodesAndEdges keys edges' =
  (graph, lookupKey)
  where
    sortedNodes = V.fromList $ L.sortOn fst keys
    edges'' = fromEdges (V.length sortedNodes) $ catMaybes (map (lookupEdge lookupKey) edges')
    graph = Graph $ V.zipWith (buildNode . snd) sortedNodes edges''
    lookupKey = binarySearch (V.map fst sortedNodes)

lookupEdge :: (key -> Maybe Vertex) -> Edge e key -> Maybe (Edge e Vertex)
lookupEdge fn (Edge k1 k2 e) =
  Edge <$> fn k1 <*> fn k2 <*> pure e

fromEdges :: Int -> [Edge e Vertex] -> V.Vector [(Vertex, e)]
fromEdges s edges' = V.create $ do
  v <- VM.replicate s []
  forM_ edges' $ \(Edge i j e) -> do
    VM.unsafeWrite v i . ((j, e):) =<< VM.unsafeRead v i
  return v


binarySearch :: Ord key => V.Vector key -> key -> Maybe Int
binarySearch v n =
  go 0 (V.length v - 1)
  where
    go !l !h
      | l <= h =
        let mid = l + ((h - l) `quot` 2)
        in case n `compare` V.unsafeIndex v mid of
          LT -> go l (mid - 1)
          GT -> go (mid + 1) h
          EQ -> Just mid
      | otherwise = Nothing
{-# INLINE binarySearch #-}

-- | Get a list of the edges in the graph.
edges :: Graph e n -> [Edge e Vertex]
edges Graph {..} =
  toListOf (ifolded.to outEdges.folded.withIndex.to (\(i, (j, e)) -> Edge i j e)) nodes
{-# INLINE edges #-}

-- | Transpose a graph
-- \( O(e) \)
transposeG  :: Graph e n -> Graph e n
transposeG g =
  Graph
  . V.imap (\i v -> buildNode (nodeLabel $ (nodes g) V.! i) v)
  $ reverseEdges g

reverseEdges :: Graph e n -> V.Vector [(Vertex, e)]
reverseEdges g = V.create $ do
  let s = V.length (nodes g)
  v <- VM.replicate s []
  iforM_ (nodes g) $ \i n -> do
    forM_ (outEdges n) $ \(j, e) -> do
      r <- VM.unsafeRead v j
      VM.unsafeWrite v j $ (i, e):r
  return v

postOrd :: Graph e n -> [Vertex]
postOrd g =
  postorderF (dff g) []
  where
    postorder (T.Node a ts) = postorderF ts . (a :)
    postorderF ts = foldr (.) id $ map postorder ts

-- | Depth first forest
dff :: Graph e n -> [T.Tree Vertex]
dff g = dfs g [0..(V.length (nodes g) -1)]

-- | Depth first search
dfs :: Graph e n -> [Vertex] -> [T.Tree Vertex]
dfs Graph{..} =
  prune . map generate
  where
    generate v =
      T.Node v . map generate . U.toList . edgeVertices $ nodes V.! v

    prune ts =
      runST $ do
       vt <- UM.replicate (V.length nodes) False
       chop vt ts

    chop :: UM.MVector s Bool -> T.Forest Vertex -> ST s (T.Forest Vertex)
    chop vt = \case
      [] -> return []
      T.Node v ts : us -> do
        visited <- UM.unsafeRead vt v
        if visited then
          chop vt us
        else do
          UM.unsafeWrite vt v True
          as <- chop vt ts
          bs <- chop vt us
          return $ T.Node v as : bs

-- | The strongly connected components of a graph.
scc'  :: Graph e n -> T.Forest Vertex
scc' g = dfs g . reverse . postOrd . transposeG $ g

-- | The strongly connected components of a graph.
scc  :: Graph e n -> [IS.IntSet]
scc = map (IS.fromList . toList) . scc'

-- | The strongly connected components of a graph.
sccN  :: Ord n => Graph e n -> [S.Set n]
sccN g = map (unsafeLabeledSet g.toList) . scc' $ g

componentMap :: Graph e n -> (V.Vector IS.IntSet, U.Vector Int)
componentMap g =
  (sccs, lookupX)
  where
    sccs = V.fromList $ scc g
    lookupX = U.create $ do
      x <- UM.new (V.length $ nodes g)
      iforM_ sccs
        $ \(i :: Int) s -> forM (IS.toList s)
        $ \j -> UM.write x j i
      return x

-- | Partition a graph into closures
partition :: Graph e n -> [ (IS.IntSet, IS.IntSet) ]
partition g =
  V.toList $ partitions
  where
    (sccs, lookupX) = componentMap g

    partitions = V.map (\is -> (is, edgesOf is)) sccs

    edgesOf is =
      IS.unions . (is:)
      . map (snd . (partitions V.!) . (lookupX U.!))
      . IS.toList
      . IS.unions
      . map (\n -> IS.fromAscList (U.toList $ edgeVertices $ nodes g V.! n) IS.\\ is)
      $ IS.toList is

-- | Partition a graph into closures
closures :: Graph e n -> [ IS.IntSet ]
closures g =
   map snd $ partition g

-- labeledSet :: Graph e n -> IS.IntSet -> S.Set n
-- labeledSet = S.fromList . map (nodeLabel . V.unsafeIndex (nodes g)) . toList

unsafeLabeledSet :: Ord n => Graph e n -> [Vertex] -> S.Set n
unsafeLabeledSet g = S.fromList . map (nodeLabel . V.unsafeIndex (nodes g))

-- | Partition a graph into closures
closuresN :: Ord n => Graph e n -> [ S.Set n ]
closuresN g =
   map (unsafeLabeledSet g . IS.toList) $ closures g

parsePretty :: Parsec Void T.Text a -> String -> T.Text -> Either String  a
parsePretty parser name bs =
#if MIN_VERSION_megaparsec(7,0,0)
  first errorBundlePretty $ parse parser name bs
#else
  first parseErrorPretty $ parse parser name bs
#endif

-- | Read TGF
readTGF :: String -> T.Text -> Either String (Graph T.Text T.Text)
readTGF = parsePretty parser
  where
    parser :: Parsec Void T.Text (Graph T.Text T.Text)
    parser = do
      nodes <- label "nodes" $ manyTill (parseNode <* eol) (space *> char '#' *> space)
      edges' <- label "edegs" $ sepEndBy parseEdge eol
      return . fst $ buildGraphFromNodesAndEdges nodes edges'

    parseNode = label "node" $ do
      skipSpace
      _name <- takeWhile1P (Just "node key") (not . isSpace)
      skipSpace
      lab <- takeWhileP (Just "node label") (/= '\n' )
      return (_name, lab)

    parseEdge = label "edge" $ do
      skipSpace
      f <- takeWhile1P (Just "edge from") (not . isSpace)
      skipSpace
      t <-  takeWhile1P (Just "edge to") (not . isSpace)
      skipSpace
      lab <- takeWhileP (Just "edge label") (/= '\n')
      return (Edge f t lab)

    skipSpace =
      void $ takeWhileP Nothing (== ' ')

instance (C.FromField e, C.FromField n) => C.FromRecord (Edge (Maybe e) n) where
  parseRecord v
    | length v == 2 =
      Edge <$> v C..! 0 <*> v C..! 1 <*> pure Nothing
    | length v == 3 =
      Edge <$> v C..! 0 <*> v C..! 1 <*> (Just <$> v C..! 2)
    | otherwise =
      mzero

instance (C.FromField e, C.FromField n) => C.FromNamedRecord (Edge (Maybe e) n) where
  parseNamedRecord m =
    Edge <$> m C..: "from" <*> m C..: "to" <*> ((Just <$> m C..: "label") <|> pure Nothing)

-- | Read a csv file of edges, given a default e to load if nothings is found in "label".
readEdgesCSV :: (C.FromField n, C.FromField e) => e -> BL.ByteString -> Either String [Edge e n]
readEdgesCSV e bs = V.toList . V.map (first $ fromMaybe e) . snd <$> C.decodeByName bs
{-# INLINE readEdgesCSV #-}

-- | Read a csv file of edges
readCSV :: (C.FromField a, C.FromField b, Ord a) => b -> [a] -> BL.ByteString -> Either String (Graph b a)
readCSV def nodes bs = do
  x <- readEdgesCSV def bs
  return . fst $ buildGraphFromNodesAndEdges (map (\a -> (a,a)) nodes) x
{-# INLINE readCSV #-}
