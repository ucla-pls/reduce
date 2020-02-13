{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Reduce.Boolean.CNF where

-- lens
import           Control.Lens

-- vector 
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM

-- primitive
import           Control.Monad.Primitive

-- base
import           Control.Monad.ST
import           Data.STRef
import           Control.Monad
import           Data.Semigroup
import           Data.Maybe
import           Text.Show
import           Data.Foldable
import           Data.Functor
-- import           Control.Applicative
import qualified Data.List                     as L

-- containers
import qualified Data.IntSet                   as IS
import qualified Data.IntMap.Strict            as IM
import qualified Data.Set                      as S

-- mtl
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans

-- reduce-util
import           Control.Reduce.Boolean
import qualified Control.Reduce.Boolean.LiteralSet
                                               as LS
import           Control.Reduce.Boolean.LiteralSet
                                                ( Clause
                                                , Term
                                                , IntLiteral
                                                , emptyClause
                                                )
import qualified Control.Reduce.Graph          as G

-- reduce
import           Control.Reduce

-- import Debug.Trace

-- | A CNF is a set of literals. A literal is a variable id, and if it is 
-- negative it will be substracted 1000.
newtype CNF = CNF { cnfClauses :: S.Set Clause }
  deriving (Show)

toCNF :: Fixed (NnfF Int) b => b -> CNF
toCNF f = CNF $ cata cnf f emptyClause
 where
  cnf = \case
    NAnd a b -> \d -> a d <> b d
    NOr a b ->
      \d -> S.fromList [ y | x <- S.toList (a d), y <- S.toList (b x) ]
    NLit   l     -> maybe S.empty S.singleton . LS.insert (LS.fromLiteral l)
    NConst True  -> const S.empty
    NConst False -> S.singleton

maxVariable :: Fixed (NnfF Int) b => b -> Int
maxVariable = cata \case
  NAnd a b           -> max a b
  NOr  a b           -> max a b
  NLit (Literal _ a) -> a
  _                  -> 0

-- TODO: Tseytin transformation
toMinimalCNF :: Fixed (NnfF Int) b => Int -> b -> CNF
toMinimalCNF maxvar f = CNF $ runST $ do
  next      <- newSTRef (maxvar + 1)
  (x, tmps) <- cata (cnf next) f emptyClause
  return $ x <> tmps
 where
  cnf nextid = \case
    NAnd a b -> \d -> do
      x <- a d
      y <- b d
      pure (x <> y)
    NOr a b -> \d -> do
      (xs, xtmps) <- a d
      if S.size xs > 1
        then do
          i           <- freshvar
          (ys, ytmps) <- b (fromJust $ tt i `LS.insert` d)
          let tmps =
                S.map (fromJust . LS.insert (ff i) . (`LS.difference` d)) xs
          pure (ys, xtmps <> ytmps <> tmps)
        else do
          (ys, ytmps) <- unzip <$> mapM b (S.toList xs)
          pure $ (S.unions ys, xtmps <> S.unions ytmps)

    NLit l ->
      \d ->
        pure
          (maybe S.empty S.singleton $ LS.insert (LS.fromLiteral l) d, S.empty)
    NConst True  -> \_ -> pure (S.empty, S.empty)
    NConst False -> \d -> pure (S.singleton d, S.empty)

   where
    freshvar = do
      a <- readSTRef nextid
      writeSTRef nextid (a + 1)
      return a


cnfVariables :: CNF -> IS.IntSet
cnfVariables = foldMap LS.variables . cnfClauses

debugCnf :: CNF -> IO ()
debugCnf cnf =
  forM_ (cnfClauses cnf) \a -> putStrLn (LS.displayImplication shows a "")

cnfSize :: CNF -> Int
cnfSize = S.size . cnfClauses

-- | Forward propergation of positive variables.
unitPropergation :: Term -> CNF -> (Maybe Term, CNF)
unitPropergation target cnf = runST $ do
  current <- V.thaw (V.map Just clauses)
  after   <- unitPropergationM
    current
    (\i -> IM.findWithDefault [] (variable . LS.toLiteral $ i) clauseLookup)
    target
  cnfResult <- V.freeze current
  return (after, CNF . fold . V.mapMaybe (fmap S.singleton) $ cnfResult)
 where
  clauses      = V.fromList . S.toList . cnfClauses $ cnf
  clauseLookup = clauseLookupMap clauses

unitPropergationM
  :: PrimMonad m
  => VM.MVector (PrimState m) (Maybe Clause)
  -- ^ The vector of clauses
  -> (IntLiteral -> [Int])
  -- ^ The variable to occurence table
  -> Term
  -> m (Maybe Term)
unitPropergationM clauses clauseLookup t = go t (LS.toList t, []) where
  go visited = \case
    (x : xs, as) -> do
      learned <- mapMaybe (>>= LS.unSingleton)
        <$> mapM (conditionClauseM clauses x) (clauseLookup x)
      case foldrM LS.insert visited learned of
        Nothing -> return Nothing
        Just visited' ->
          go visited' (xs, filter (`LS.notMember` visited) learned ++ as)
    ([], []) -> return (Just visited)
    ([], as) -> go visited (reverse as, [])
{-# INLINE unitPropergationM #-}

-- | Condition a clause with a IntLiteral
conditionClauseM
  :: PrimMonad m
  => VM.MVector (PrimState m) (Maybe Clause)
  -> IntLiteral
  -- ^ Clause Id
  -> Int
  -> m (Maybe Clause)
conditionClauseM clauses il i = VM.read clauses i >>= \case
  Nothing -> return Nothing
  Just (LS.conditionClause il -> clause) -> do
    VM.write clauses i clause
    return clause

clauseLookupMap :: V.Vector Clause -> IM.IntMap [Int]
clauseLookupMap clauses =
  IM.map (flip appEndo [])
    .   IM.unionsWith (<>)
    $   V.indexed clauses
    <&> \(i, c) -> IM.fromSet (const $ Endo (i :)) (LS.variables c)

vmapCNF :: (Int -> Int) -> CNF -> CNF
vmapCNF fn = CNF . S.fromList . mapMaybe (LS.vmap fn) . S.toList . cnfClauses

-- | Compress a CNF
-- Given a cnf, compute the closures of the sure units, then compress those
-- into single variables. This will both reduce the number of variables in
-- CNF, and it will also sort the variables accendingly after cost. 
compressCNF
  :: IS.IntSet
  -- ^ Variables
  -> (IS.IntSet -> Double)
  -- ^ Cost function
  -> CNF
  -- ^ CNF
  -> (CNF, V.Vector IS.IntSet)
compressCNF vars cost cnf = (vmapCNF (variableMap IM.!) cnf, compression)
 where
  arrows     = cnfDependencies cnf
  (graph, _) = G.buildGraphFromNodesAndEdges
    (map (\a -> (a, a)) $ IS.toList (cnfVariables cnf <> vars))
    (map (\(f, t) -> G.Edge () f t) arrows)

  variableMap = ifoldMap (\i a -> IM.fromSet (const i) a) compression

  compression =
    V.fromList
      . map fst
      . L.sortOn (cost . snd)
    -- sort on cost of closures
      . L.map (over both (IS.map (G.nodeLabel . V.unsafeIndex (G.nodes graph))))
    -- map back into our space
      $ G.partition graph

cnfDependencies :: CNF -> [(Int, Int)]
cnfDependencies =
  mapMaybe (both unSingleton . LS.splitLiterals) . S.toList . cnfClauses
 where
  unSingleton s = case IS.minView s of
    Just (s', xs) | IS.null xs -> Just s'
    _                          -> Nothing


-- | Implicative Positive Form, 
-- The requirements is that all clauses contain at least one possitive
-- varaible.
-- All singleton positive variables are split from the set.
data IPF = IPF 
  { ipfClauses :: CNF
  , ipfVars    :: IS.IntSet 
  , ipfFacts   :: IS.IntSet
  } deriving (Show)

debugIpf :: IPF -> IO ()
debugIpf = debugIpfWith shows

debugIpfWith :: (Int -> ShowS) -> IPF -> IO ()
debugIpfWith fn (IPF cnf vars facts) = do
  putStrLn $ "Vars:  " ++ showListWith fn (IS.toList vars) ""
  putStrLn $ "Facts: " ++ showListWith fn (IS.toList facts) ""
  forM_ (cnfClauses cnf) \a -> putStrLn (LS.displayImplication fn a "")

fromCNF :: CNF -> Maybe IPF
fromCNF cnf | any (IS.null . snd . LS.splitLiterals) $ cnfClauses cnf = Nothing
            | otherwise = Just (IPF cnf (cnfVariables cnf) IS.empty)

conditionCNF :: IS.IntSet -> CNF -> CNF
conditionCNF is = CNF
  . S.fromList
  . mapMaybe (\m -> IS.foldr (\a c -> c >>= LS.conditionClause (tt a)) (Just m) is)
  . S.toList
  . cnfClauses 

-- 
-- | Conditioning an IPF with positive literals produce a IPF.
conditionIPF :: IS.IntSet -> IPF -> IPF
conditionIPF is (IPF cnf vars facts) = 
  if IS.null falses 
  then IPF cnf' vars (facts `IS.union` trues)
  else error $ "unexpected: " ++ show is ++ " " ++ show falses
  where 
    -- unit propergation on ipfs are safe
    (LS.splitLiterals . fromJust -> (falses, trues), cnf') 
      = unitPropergation (LS.joinLiterals' (IS.empty, is)) cnf

-- | Given a set of positive varaibles that is a true assignent to the
-- problem we can create an IPF that is limted to only those variables.
limitIPF' :: IS.IntSet -> IPF -> IPF
limitIPF' is (IPF cnf _ facts) =
  IPF 
    (CNF . S.fromList . mapMaybe (LS.limitClause is) . S.toList $ cnfClauses cnf)
    is
    facts

learnClauseIPF :: IS.IntSet -> IPF -> IPF
learnClauseIPF is ipf 
  | IS.size is == 0 = error "can't learn an empty clause"
  | IS.size is == 1 = conditionIPF is ipf
  | otherwise       = ipf 
    { ipfClauses = CNF 
      ( S.insert (LS.joinLiterals' (IS.empty, is)) 
      $ cnfClauses (ipfClauses ipf)
      ) 
    }
 
-- compressAndProgress :: 
--   IS.IntSet 
--   -> (IS.IntSet -> Double) 
--   -> CNF 
--   -> [IS.IntSet]
-- compressAndProgress vars cost cnf =
--   [ foldMap (\i -> back V.! i) (IS.toList set)
--   | set <- positiveProgression (IS.fromDistinctAscList [0..V.length back - 1]) cnf'
--   ]
--   where 
--     (cnf', back) = compressCNF vars cost cnf


weightedSubIPFs :: 
  (IS.IntSet -> Double) 
  -> IPF 
  -> ((IS.IntSet, IPF), V.Vector (IS.IntSet, IPF))
weightedSubIPFs cost ipf@(IPF cnf vars facts) =
  ( let con = foldMap (\i -> back V.! i) $ IS.toList minimal 
    in (con, limitIPF' (ipfFacts ipf `IS.union` con) ipf)
  , V.map snd 
    . V.postscanr (\s (ipf', _) -> 
        let con = foldMap (\i -> back V.! i) $ IS.toList s
        in ( limitIPF' (ipfVars ipf' `IS.difference` con) ipf'
           , (con, learnClauseIPF con ipf')
           )
        ) 
      (ipf, undefined)
    $ V.fromList choices
  )
 where
  -- (knowns, pclauses, bipf) = splitIPF ipf
  (cnf', back) = compressCNF (vars `IS.difference` facts) cost cnf

  (minimal, choices) = 
    let (m, clauses') = findMinimum (V.fromList . S.toList . cnfClauses $ cnf')
    in (m, go (thvars `IS.difference` minimal) clauses')
    where 
      thvars = IS.fromList [0..V.length back -1] 
  
  go vs clauses = case IS.minView vs of
    Just (p, _) ->
      let (term, clauses') = positiveResolution p clauses
      in  term : go (vs `IS.difference` term) clauses'
    Nothing -> []

  -- Find something that satisfy the results
  findMinimum :: V.Vector Clause -> (IS.IntSet, V.Vector Clause)
  findMinimum clauses = case IS.minView (freeAgents clauses) of
    Just (x, _) -> 
      let (required, rest) = positiveResolution x clauses
      in over _1 (IS.union required) $ findMinimum rest
    Nothing     -> (IS.empty, clauses)
   where
    freeAgents = foldMap \c ->
      let (trues, falses) = LS.splitLiterals c
      in  if IS.null trues then falses else IS.empty


  positiveResolution :: Int -> V.Vector Clause -> (IS.IntSet, V.Vector Clause)
  positiveResolution target clauses = runST $ do
    current <- V.thaw (V.map Just clauses)
    let clauseLookup = clauseLookupMap clauses
    after' <- unitPropergationM
      current
      (\i -> IM.findWithDefault [] (variable . LS.toLiteral $ i) clauseLookup)
      (LS.singleton (tt target))
    cnfResult <- V.freeze current
    return
      (snd . LS.splitLiterals . fromJust $ after', V.mapMaybe id cnfResult)

binarySearchV :: MonadPlus m => (x -> m ()) -> V.Vector x -> m x
binarySearchV p as = do
  (as V.!) <$> binarySearch (p . (as V.!)) 0 (V.length as - 1)

ipfBinaryReduction
  :: (Monad m) => (IS.IntSet -> Double) -> Reducer m IPF
ipfBinaryReduction cost ((\p -> lift . p >=> guard) -> p) = runMaybeT . go where
  go ipf@(weightedSubIPFs cost -> (a, rest)) = msum
    [ takeIfSolution (snd a)
    , if Prelude.not $ V.null rest then
         binarySearchV p (V.map snd rest) >>= \ipf' -> go ipf'
      else mzero
    , return ipf
    ]
  takeIfSolution a = p a $> a

