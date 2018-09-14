
import           System.IO
import           System.Environment
import           System.Random
import           Control.Monad.Writer
import           Control.Monad.Trans.State (StateT, runStateT, runState)
import           Control.Monad.State.Class
import           Control.Monad
import           Data.Monoid
import           Data.Maybe (catMaybes)

import qualified Data.List as L
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.Vector as V
import           Control.Monad.Trans.Maybe

import           Control.Reduce

import qualified Data.Graph.Inductive.Graph as F
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.DFS

import           Debug.Trace

main = do
  hSetBuffering stdout LineBuffering
  cmd:xs <- getArgs
  case cmd of
    "base" ->
      base xs
    "setofsets" ->
      setsofsets xs
    "graph" ->
      graph xs


graph [(read -> n), (read -> maxe), (read -> s)] = do
  putStrLn . L.intercalate "," $
     "N":(concatMap (\(name,_) -> [name ++ "-steps", name ++ "-size"]) reds)
  forever $ do
    (graph, mini) <- getStdRandom . runState $ do
      space <- forM [0..n-1] $ \i -> do
        g <- randomSTR (0, min i maxe)
        fmap toIS . takeN g S.empty $ randomSTR (0, i - 1)
      let sV = V.fromList . IS.toList $ IS.unions space
      minimum <- fmap toIS . takeN s S.empty $ randomSTR (0, n - 1)
      return (mkGraph space, minimum)

    x <- fmap concat . forM reds $ \(name,red) -> do
          (Just f, x) <- red (closures graph) mini
          return [ show x
                 , show (IS.size . IS.unions $ f)
                 ]
    putStrLn $ L.intercalate "," ((show $ n):x)

setsofsets [(read -> n), (read -> setsize), (read -> s)] = do
  putStrLn . L.intercalate "," $
     "N":(concatMap (\(name,_) -> [name ++ "-steps", name ++ "-size"]) reds)
  forever $ do
    (space, sV, mini) <- getStdRandom . runState $ do
      space <- fmap S.toList . takeN n S.empty $ do
        g <- randomSTR (0, setsize)
        fmap toIS . takeN g S.empty $ randomSTR (0, n - 1)
      let sV = V.fromList . IS.toList $ IS.unions space
      minimum <- takeN s S.empty $ randomSTR (0, V.length sV - 1)
      return (space, sV, foldMap (IS.singleton . V.unsafeIndex sV) minimum)

    x <- fmap concat . forM reds $ \(name,red) -> do
          (Just f, x) <- red space mini
          return $ [ show x
                   , show (IS.size . IS.unions $ f)
                   ]
    putStrLn $ L.intercalate "," ((show $ V.length sV):x)

reds =
  [ ("ddmin", runSetCase "ddmin" . toSetReducer $ ddmin)
  , ("BiRed", runSetCase "bired" . toSetReducer $ binaryReduction)
  , ("GBiRed", runSetCase "GBiRed" . toSetReducer $ genericBinaryReduction (IS.size . IS.unions))
  , ("sBiRed", runSetCase "sBiRed" setBinaryReduction)
  ]
  where
    runSetCase ::
      String
      -> ISetReducer (StateT Int IO)
      -> [IS.IntSet]
      -> IS.IntSet
      -> IO (Maybe [IS.IntSet], Int)
    runSetCase str red space mini = do
      runStateT (red pred space) 0
      where
        pred r = do
          modify (+1)
          return $ mini `IS.isSubsetOf` r

base xs = do
  let mkBenchmark = parseArgs xs
  putStrLn $  L.intercalate "," ["N", "S", "ddmin", "BiRed"]
  forever $ do
    b <- mkBenchmark
    let r = runBenchCase b
    putStrLn . L.intercalate "," . map show $ r
  where
    parseArgs ("N":[(read -> maxn), (read -> s)]) =
      mkRandomScaleN maxn s
    parseArgs ("SeqN":[(read -> maxn), (read -> s)]) =
      mkRandomScaleSeqN maxn s
    parseArgs ("S":[(read -> maxs), (read -> n)]) =
      mkRandomScaleS maxs n
    parseArgs ("SeqS":[(read -> maxs), (read -> n)]) =
      mkRandomScaleSeqS maxs n
    parseArgs ("Dist":[(read -> n), (read -> s)]) =
      mkRandomScaleDist s n
    parseArgs args =
      error $ "Could not parse " ++ show args

runBenchCase b = do
  ([bcN b,bcS b] ++)
    . map (\r -> if resMinimal r then L.length (resTries r) else -1)
    . map (\x -> runCase x [0..(bcN b)] (bcMinima b))
    $ [ddmin, binaryReduction]


data BenchmarkCase = BenchmarkCase
  { bcN :: Int
  , bcS :: Int
  , bcMinima :: [[Int]]
  } deriving (Show, Eq)

mkRandomScaleN ::
  Int ->
  Int ->
  IO BenchmarkCase
mkRandomScaleN maxn s =
  getStdRandom . runState $ do
    n :: Int <- randomSTR (s, maxn)
    set <- takeN s S.empty $ randomSTR (0, n)
    return $! BenchmarkCase n s [S.toList set]

randomSTR :: (MonadState StdGen m, Random a) => (a, a) -> m a
randomSTR = state . randomR

randomSTRs :: (MonadState StdGen m, Random a) => (a, a) -> Int -> m [a]
randomSTRs r 0 = return []
randomSTRs r n =
  (:) <$> randomSTR r <*> randomSTRs r (n - 1)

mkRandomScaleS ::
  Int ->
  Int ->
  IO BenchmarkCase
mkRandomScaleS maxs n =
  getStdRandom . runState $ do
    s :: Int <- randomSTR (0, maxs)
    set <- takeN s S.empty $ randomSTR (0, n)
    return $! BenchmarkCase n s [S.toList set]

mkRandomScaleSeqN ::
  Int ->
  Int ->
  IO BenchmarkCase
mkRandomScaleSeqN maxn s =
  getStdRandom . runState $ do
    n :: Int <- randomSTR (s, maxn)
    sstart <- randomSTR (0, n-s)
    return $! BenchmarkCase n s [[sstart..sstart+s]]

mkRandomScaleSeqS ::
  Int ->
  Int ->
  IO BenchmarkCase
mkRandomScaleSeqS maxs n =
  getStdRandom . runState $ do
    s :: Int <- randomSTR (0, maxs)
    sstart <- randomSTR (0, n-s)
    return $! BenchmarkCase n s [[sstart..sstart+s]]

mkRandomScaleDist ::
  Int ->
  Int ->
  IO BenchmarkCase
mkRandomScaleDist s n = do
  (cuts, items, ini:rest) <- getStdRandom . runState $ do
    cuts :: Int <- randomSTR (1, s)
    items <- distribute (s - cuts) cuts
    spaces <- distribute (n - s - cuts + 1) (cuts + 1)
    return (cuts, items, spaces)

  let res = scanl (\(off, _) (i, space) -> (off + i + 1 + space + 1, [off..off+i+1])) (ini, [])
              $ zip items rest

  return $! BenchmarkCase n cuts [concatMap snd res]

distribute n c = do
   pieces <- randomSTRs (0, 1) c
   let ps :: [Double] = [ fromIntegral n * (p / sum pieces) | p <- pieces ]
   return . map snd.  tail $
      L.scanl
        (\(v, _) p -> let x = floor (v + p) in (v + p - fromIntegral x, x))
        (0, 0)
        ps

basic = do
  runWithAll "one" all [0..99] [[54]]
  runWithAll "two" all [0..99] [[23, 74]]
  runWithAll "three" all [0..99] [[23, 50, 74]]
  runWithAll "contiguous-small" all [0..99] [[15..25]]
  runWithAll "contiguous-large" all [0..99] [[15..75]]
  runWithAll "spread-large" all [0..99] [[0,2..99]]
  runWithAll "all" all [0..99] [[0..99]]
  where
    all = [ ("ddmin", ddmin)
          , ("bired", binaryReduction)
          , ("bireds", (\p i -> fmap fst . L.uncons <$> binaryReductions p i ))
          , ("lired", linaryReduction)
          ]

runWithAll ::
  String
  -> [(String, TestReducer)]
  -> [Int]
  -> [[Int]]
  -> IO ()
runWithAll casename ps xs minima =
  forM_ ps $ \(name, red) -> do
    let r = runCase red xs minima
    putStrLn $ L.intercalate ","
      [ casename, name, show (L.length (resTries r)), show (resMinimal r)]


type TestReducer = Reducer [Int] ((,) (Endo [(Bool,Int)]))

data Result = Result
  { resTries :: [(Bool, Int)]
  , resMinimal :: Bool
  } deriving (Show)

runCase ::
  TestReducer
  -> [Int]
  -> [[Int]]
  -> Result
runCase red space minima =
  let
    (m, res) = red pred space
  in
    Result
    { resTries = appEndo m []
    , resMinimal =
      case res of
        Just x -> any (x ==) minima
        Nothing -> False
    }
  where
    pred :: forall m. MonadWriter (Endo [(Bool, Int)]) m => [Int] -> m Bool
    pred xs = do
      let t = hasMinima xs
      tell (Endo ((t, L.length xs):))
      return t
    hasMinima xs = any (flip L.isSubsequenceOf xs) minima


takeN :: (Monad m, Ord a) => Int -> S.Set a -> m a -> m (S.Set a)
takeN n set mx
  | S.size set >= n =
    return set
  | otherwise = do
    x <- mx
    takeN n (S.insert x set) mx

toIS :: S.Set Int -> IS.IntSet
toIS = IS.fromAscList . S.toAscList


mkGraph :: [IS.IntSet] -> Gr () ()
mkGraph adj =
  F.mkGraph [ (i, ()) | i <- [0..(L.length adj -1)]] (concat edges)
  where
    edges = zipWith (\i -> map (i,,()) . IS.toList) [0..] adj


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
