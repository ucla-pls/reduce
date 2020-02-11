{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Reduce.Boolean.CNF where

-- lens
import Control.Lens

-- vector 
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- primitive
import Control.Monad.Primitive

-- base
import Control.Monad.ST 
import Data.STRef
import Data.Semigroup
import Data.Maybe
import Data.Foldable
import qualified Data.List as L

-- containers
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

-- reduce-util
import Control.Reduce.Boolean
import qualified Control.Reduce.Boolean.LiteralSet as LS
import Control.Reduce.Boolean.LiteralSet (Clause, Term, IntLiteral, emptyClause)
import qualified Control.Reduce.Graph as G


-- | A CNF is a set of literals. A literal is a variable id, and if it is 
-- negative it will be substracted 1000.
newtype CNF = CNF { cnfClauses :: S.Set Clause }
  deriving (Show)

toCNF :: Fixed (NnfF Int) b => b -> CNF
toCNF f =
  CNF $ cata cnf f emptyClause
 where 
  cnf = \case
    NAnd a b -> \d -> a d <> b d
    NOr a b -> \d -> S.fromList [ y | x <- S.toList (a d), y <- S.toList (b x)]
    NLit l -> maybe S.empty S.singleton . LS.insert (LS.fromLiteral l)
    NConst True -> const S.empty
    NConst False -> S.singleton 
 
maxVariable :: Fixed (NnfF Int) b => b -> Int
maxVariable = cata \case 
  NAnd a b -> max a b
  NOr a b  -> max a b
  NLit (Literal _ a)  -> a
  _ -> 0

toMinimalCNF :: Fixed (NnfF Int) b => Int -> b -> CNF
toMinimalCNF maxvar f =
  CNF $ runST $ do 
    next <- newSTRef (maxvar + 1)
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
        i <- freshvar 
        (ys, ytmps) <- b (fromJust $ tt i `LS.insert` d)
        let tmps = S.map (fromJust . LS.insert (ff i) . (`LS.difference` d)) xs
        pure (ys, xtmps <> ytmps <> tmps)
      else do
        (ys, ytmps) <- unzip <$> mapM b (S.toList xs)
        pure $ (S.unions ys, xtmps <> S.unions ytmps)

    NLit l -> \d -> pure (maybe S.empty S.singleton $ LS.insert (LS.fromLiteral l) d, S.empty)
    NConst True -> \_ -> pure (S.empty, S.empty)
    NConst False -> \d -> pure (S.singleton d, S.empty)

   where
     freshvar = do
      a <- readSTRef nextid
      writeSTRef nextid (a + 1) 
      return a


cnfVariables :: CNF -> IS.IntSet
cnfVariables =
  foldMap LS.variables . cnfClauses

debugCnf :: CNF -> IO ()
debugCnf cnf = 
  forM_ (cnfClauses cnf) \a -> putStrLn (LS.displayImplication shows a "")

cnfSize :: CNF -> Int
cnfSize = S.size . cnfClauses

-- | Forward propergation of positive variables.
unitPropergation :: Term -> CNF -> (Maybe Term, CNF)
unitPropergation target cnf = runST $ do
  current <- V.thaw (V.map Just clauses)
  after <- unitPropergationM current 
    (\i -> IM.findWithDefault [] (variable . LS.toLiteral $ i) clauseLookup)
    target
  cnfResult <- V.freeze current
  return (after, CNF . fold . V.mapMaybe (fmap S.singleton) $ cnfResult)
 where
  clauses = V.fromList . S.toList . cnfClauses $ cnf
  clauseLookup = clauseLookupMap clauses 

unitPropergationM :: 
  PrimMonad m 
  => VM.MVector (PrimState m) (Maybe Clause) 
  -- ^ The vector of clauses
  -> (IntLiteral -> [Int])
  -- ^ The variable to occurence table
  -> Term
  -> m (Maybe Term)
unitPropergationM clauses clauseLookup t = go t (LS.toList t, []) where 
  go visited = \case 
    (x:xs, as) -> do
      learned <- mapMaybe (>>= LS.unSingleton) 
        <$> mapM (conditionClauseM clauses x) (clauseLookup x)
      case foldrM LS.insert visited learned of
        Nothing -> return Nothing
        Just visited' -> go visited' (xs, filter (`LS.notMember` visited) learned ++ as)
    ([],[]) -> return (Just visited)
    ([],as) -> go visited (reverse as, [])
{-# INLINE unitPropergationM #-}

-- | Condition a clause with a IntLiteral
conditionClauseM :: 
  PrimMonad m 
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
  IM.map (flip appEndo []) . IM.unionsWith (<>) 
    $ V.indexed clauses <&> \(i, c) -> 
    IM.fromSet (const $ Endo (i:)) (LS.variables c)

-- Given a set of variables, and a CNF calculate a 
positiveProgression :: IS.IntSet -> CNF -> [IS.IntSet]
positiveProgression vs' cnf = go vs' (V.fromList . S.toList . cnfClauses $ cnf) where 
  go vs clauses = case IS.minView vs of 
    Just (p, _) -> 
      let (term, clauses') = resolve (IS.singleton p) clauses
      in term : go (vs `IS.difference` term) clauses'
    Nothing -> []

  -- Find something that satisfy the results
  resolve target clauses = 
    case IS.minView freeAgents of 
      Just (x, _) -> 
        over _1 (IS.union after) 
        $ resolve (IS.singleton x) results
      Nothing -> 
        (after, results)
    where
      freeAgents = flip foldMap results \c -> 
        let (trues, falses) = LS.splitLiterals c 
        in if IS.null trues then falses else IS.empty

      (after, results) = runST $ do 
        current <- V.thaw (V.map Just clauses)
        let clauseLookup = clauseLookupMap clauses
        after' <- unitPropergationM current 
          (\i -> IM.findWithDefault [] (variable . LS.toLiteral $ i) clauseLookup)
          (fromJust (LS.fromList (map tt . IS.toList $ target)))
        cnfResult <- V.freeze current
        return (snd . LS.splitLiterals . fromJust $ after', V.mapMaybe id cnfResult)


cnfDependencies :: CNF -> [(Int, Int)]
cnfDependencies = mapMaybe (both unSingleton . LS.splitLiterals)
  . S.toList
  . cnfClauses
 where
  unSingleton s = case IS.minView s of 
    Just (s', xs) | IS.null xs -> Just s'
    _ -> Nothing


  
vmapCNF :: 
  (Int -> Int)
  -> CNF 
  -> CNF
vmapCNF fn =
  CNF . S.fromList . mapMaybe (LS.vmap fn) 
  . S.toList . cnfClauses


-- | Compress a CNF
-- Given a cnf, compute the closures of the sure units, then compress those
-- into single variables. This will both reduce the number of variables in
-- CNF, and it will also sort the variables accendingly after cost. 
compressCNF :: 
  IS.IntSet 
  -- ^ Variables
  -> (IS.IntSet -> Double) 
  -- ^ Cost function
  -> CNF 
  -- ^ CNF
  -> (CNF, V.Vector IS.IntSet)
compressCNF vars cost cnf = (vmapCNF (variableMap IM.!) cnf, compression)
 where
  arrows = cnfDependencies cnf
  (graph, _) = G.buildGraphFromNodesAndEdges 
    (map (\a -> (a,a)) $ IS.toList (cnfVariables cnf <> vars))
    (map (\(f,t) -> G.Edge () f t) arrows)

  variableMap = 
    ifoldMap (\i a -> IM.fromSet (const i) a) 
    compression

  compression = 
    V.fromList 
    . map fst
    . L.sortOn (cost . snd) 
    -- sort on cost of closures
    . L.map (over both (IS.map (G.nodeLabel . V.unsafeIndex (G.nodes graph))))
    -- map back into our space
    $ G.partition graph






