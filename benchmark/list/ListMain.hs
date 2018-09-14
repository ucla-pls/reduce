
import           Control.Reduce

import qualified Data.IntSet as IS
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe

import qualified Data.Graph.Inductive.Graph as F
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.DFS

main = do
  let gr = testcase
  -- x <- unsafeDdmin (prop gr) [0..16]

  let cc = closures gr

  -- x <- unsafeDdmin (propCC input cc) $ input

  -- Figure 6
  let input = [0..7] -- L.sortOn (IS.size . (cc L.!!)) [0..7]
  x <- genericBinaryReduction (const 0) (propCC (IS.member 1) input cc) $ input

  putStrLn " ------ 8< ------- "
  -- Figure 7
  let input = L.sortOn (IS.size . (cc L.!!)) [0..7]
  x <- genericBinaryReduction (\lst -> IS.size $ IS.unions [ cc L.!! l | l <- lst ])
        (propCC (IS.member 1) input cc) $ input

  putStrLn " ------ 8< ------- "
  -- Figure 8

  let input = L.sortOn (IS.size . (cc L.!!)) [0..7]
  x <- genericBinaryReduction (\lst -> IS.size $ IS.unions [ cc L.!! l | l <- lst ])
         (propCC ((&&) <$> IS.member 1 <*> IS.member 12) input cc) $ input

  return ()

  where
    prop gr lst = do
      putStrLn $ L.intercalate " & " (
        [ if L.elem x lst then
            (if isOne && isClosed then "$\\square$" else "$\\blacksquare$") else "$\\cdot$" | x <- [0..16] ] ++
        [ if not isClosed then "?" else if isOne then "$\\cmark$" else "$\\xmark$"]) ++ "\\\\"
      return (isOne && isClosed)
      where
        isOne = L.elem 1 lst
        isClosed = isClosedIn lst gr

    propCC f input cc lst = do
      putStrLn $ L.intercalate " & " (
        [ if L.elem x lst then
            (if isOne then "$\\square$" else "$\\blacksquare$")
          else "$\\cdot$" | x <- input ] ++
        [ if isOne then "$\\cmark$" else "$\\xmark$"]) ++ "\\\\"
      return isOne
      where
        isOne = f (IS.unions [ cc L.!! l | l <- lst ])

testcase =
  mkGraph
    [ IS.empty
    , IS.fromList [2,4,7]
    , IS.fromList [1,4,7]
    , IS.fromList [1,7]
    , IS.fromList [7]
    , IS.fromList [4,6,3]
    , IS.fromList [4,5,7]
    , IS.fromList []
    , IS.fromList [7,9,10,11,12,13]
    , IS.fromList [8,10]
    , IS.fromList [8]
    , IS.fromList [13]
    , IS.fromList [13]
    , IS.fromList [7,8,10,14]
    , IS.fromList [8,10,13]
    , IS.fromList [7,8,10,12,13,16]
    , IS.fromList [7,12,13,15]
    ]

mkGraph :: [IS.IntSet] -> Gr () ()
mkGraph adj =
  F.mkGraph [ (i, ()) | i <- [0..(L.length adj -1)]] (concat edges)
  where
    edges = zipWith (\i -> map (i,,()) . IS.toList) [0..] adj

isClosedIn ::
  [Int]
  -> Gr v e
  -> Bool
isClosedIn vs gr =
  closure == input
  where
    input = (L.sort $ vs)
    closure = L.sort $ dfs vs gr


closures :: Gr v e -> [ IS.IntSet ]
closures graph =
  catMaybes . V.toList $ cv
  where
    sccs = scc graph
    iscc :: [(Int,[Int])]
    iscc = zip [0..] sccs
    vMap = IM.fromList . concatMap (\(i,xs) -> map (,i) xs) $ iscc

    edges = map (\(i, ls) -> IS.unions . map (IS.delete i . getEdges) $ ls) iscc

    cv = V.fromList (zipWith getNode sccs edges)

    getEdges =
      IS.fromList
      . map (\(_,b,_) -> vMap IM.! b)
      . F.out graph

    getNode :: [Int] -> IS.IntSet -> Maybe IS.IntSet
    getNode s t = do
      let
        Just before = sequence [ cv V.! b | b <- IS.toList t ]
        s'  = IS.fromList s
        closure = IS.unions (s':before)
      return closure
