{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Control.Reduce.Boolean.CNF where

-- lens
import           Control.Lens
import           Data.Set.Lens

-- parsec
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- vector
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM

-- primitive
import           Control.Monad.Primitive

-- base
import           Control.Monad.ST
import           Data.STRef
-- import           Text.Printf
import           Control.Monad
import           Data.Semigroup
import           Data.Bifunctor
import           Data.Maybe
import           Data.Tuple
import           Data.Void
import           Data.Either
import           Data.Foldable
-- import           Control.Applicative
import           Text.Show
import           Data.Functor
import qualified Data.List                     as L
import qualified Data.List.NonEmpty            as NE

-- containers
import qualified Data.IntSet                   as IS
import qualified Data.IntMap.Strict            as IM
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S

-- text
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as LazyText
import qualified Data.Text.Lazy.IO             as LazyText

-- mtl
import           Control.Monad.Trans.Maybe

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

-- | A CNF is a set of clauses.
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

subsume :: S.Set Clause -> S.Set Clause
subsume = S.fromList . go . L.sortOn LS.size . S.toList where
  go = \case
    x : rest ->
      x : go (L.filter (\c -> Prelude.not (x `LS.isSubsetOf` c)) rest)
    [] -> []

-- | Negate all literals
transpose :: CNF -> CNF
transpose = CNF . S.map LS.transpose . cnfClauses

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
      (xs', xtmps) <- a d
      let xs = subsume xs'
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
debugCnf = debugCnfWith shows

debugCnfWith :: (Int -> ShowS) -> CNF -> IO ()
debugCnfWith fn cnf =
  forM_ (cnfClauses cnf) \a -> putStrLn (LS.displayImplication fn a "")

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

-- | Shrink a CNF
shrinkCNF
  :: IS.IntSet
  -- ^ The variables
  -> CNF
  -- ^ CNF
  -> (CNF, IM.IntMap Int, V.Vector Int)
shrinkCNF vars cnf = (vmapCNF (variableMap IM.!) cnf, variableMap, compression)
 where
  variableMap = ifoldMap (\i a -> IM.singleton a i) compression
  compression = V.fromList (IS.toList $ cnfVariables cnf <> vars)

cnfDependencies :: CNF -> [(Int, Int)]
cnfDependencies =
  mapMaybe (both unSingleton . LS.splitLiterals) . S.toList . cnfClauses
 where
  unSingleton s = case IS.minView s of
    Just (s', xs) | IS.null xs -> Just s'
    _                          -> Nothing


isIPF :: CNF -> Bool
isIPF = all LS.hasPositiveClause . cnfClauses

isDualIPF :: CNF -> Bool
isDualIPF = all LS.hasNegativeClause . cnfClauses

nonNegativeClausesVariables :: CNF -> [IS.IntSet]
nonNegativeClausesVariables =
  mapMaybe nonNegativeClause . S.toList . cnfClauses
 where
  nonNegativeClause c = do
    let (ff, tt) = LS.splitLiterals c
    if IS.null ff then Just tt else Nothing




-- | Implicative Positive Form,
-- The requirements is that all clauses contain at least one possitive
-- varaible.
-- All singleton positive variables are split from the set.
data IPF = IPF
  { ipfClauses :: CNF
  , ipfFacts   :: IS.IntSet
  } deriving (Show)

debugIpf :: IPF -> IO ()
debugIpf = debugIpfWith shows

debugIpfWith :: (Int -> ShowS) -> IPF -> IO ()
debugIpfWith fn (IPF cnf facts) = do
  putStrLn $ "Facts: " ++ showListWith fn (IS.toList facts) ""
  forM_ (cnfClauses cnf) \a -> putStrLn (LS.displayImplication fn a "")

unitResolve :: CNF -> Maybe (Term, CNF)
unitResolve cnf = do
  singletons <- LS.fromList . mapMaybe LS.unSingleton . S.toList $ cnfClauses
    cnf
  let (mterm, cnf') = unitPropergation singletons cnf
  term <- mterm
  return (term, cnf')


fromCNF :: CNF -> Maybe IPF
fromCNF cnf
  | any (IS.null . snd . LS.splitLiterals) $ cnfClauses cnf = Nothing
  | otherwise = unitResolve cnf
  <&> \(LS.splitLiterals -> (_, trues), cnf') -> (IPF cnf' trues)

conditionCNF :: IS.IntSet -> CNF -> CNF
conditionCNF is =
  CNF
    . S.fromList
    . mapMaybe
        (\m -> IS.foldr (\a c -> c >>= LS.conditionClause (tt a)) (Just m) is)
    . S.toList
    . cnfClauses

--
-- | Conditioning an IPF with positive literals produce a IPF.
conditionIPF :: IS.IntSet -> IPF -> IPF
conditionIPF is (IPF cnf facts) = if IS.null falses
  then IPF cnf' (facts `IS.union` trues)
  else error $ "unexpected: " ++ show is ++ " " ++ show falses
 where
    -- unit propergation on ipfs are safe
  (LS.splitLiterals . fromJust -> (falses, trues), cnf') =
    unitPropergation (LS.joinLiterals' (IS.empty, is)) cnf

-- -- | Given a set of positive varaibles that is a true assignent to the
-- -- problem we can create an IPF that is limted to only those variables.
-- limitIPF' :: IS.IntSet -> IPF -> IPF
-- limitIPF' is (IPF cnf facts) =
--   IPF
--     (CNF . S.fromList . mapMaybe (LS.limitClause is) . S.toList $ cnfClauses cnf)
--     (is `IS.union` facts)
--     facts
--
-- limitIPF'' :: IS.IntSet -> IPF -> Maybe IPF
-- limitIPF'' is (IPF cnf _ facts) =
--   let cnf' = CNF . S.fromList . mapMaybe (LS.limitClause is) . S.toList $ cnfClauses cnf
--   in removeSingletons cnf' <&> \(t, cnf'') ->
--     IPF cnf''
--     (is `IS.union` facts)
--     (facts `IS.union` snd (LS.splitLiterals t))

learnClauseIPF :: IS.IntSet -> IPF -> IPF
learnClauseIPF is ipf
  | IS.size is == 0 = error "can't learn an empty clause"
  | IS.size is == 1 = conditionIPF is ipf
  | otherwise = ipf
    { ipfClauses =
      CNF
        ( S.insert (LS.joinLiterals' (IS.empty, is))
        $ cnfClauses (ipfClauses ipf)
        )
    }

-- fastLWCC ::
--   (IS.IntSet -> Double)
--   -> IPF
--   -> IS.IntSet
--   -> IS.IntSet
-- fastLWCC cost ipf input =
--   facts `IS.union` unmap (minimizeCNF (V.length back) (V.fromList . S.toList . cnfClauses $ cnf'))
--  where
--   (IPF cnf facts) = conditionIPF input ipf
--   unmap = foldMap (\i -> back V.! i) . IS.toList
--   (cnf', back) = compressCNF (vars `IS.difference` facts) cost cnf
--

-- -- | Calculate the Logical Closure from an set.
-- logicalClosure ::
--   IPF
--   -- ^ the ipf
--   -> IS.IntSet
--   -- ^ valid variables
--   -> IS.IntSet
--   -- ^ input set
--   -> IS.IntSet
-- logicalClosure (IPF cnf facts) vars = \input -> IS.unions
--   [ input
--   , facts
--   , let
--       requiredItems = mapMaybe (there IM.!?) $ IS.toList input
--       closure = minimizeCNF'
--         lookupC
--         (facts' ++ requiredItems, options')
--         (V.length back)
--         clauses
--     in unmap closure
--   ]
--
--  where
--   unmap =
--     foldMap (\i -> IS.singleton $ back V.! i) . IS.toList
--
--   (cnf', there, back) =
--     shrinkCNF (vars `IS.difference` facts) cnf
--
--   (lookupC, (facts', options')) =
--     initializePropergation (V.length back) clauses
--
--   clauses =
--     (V.fromList . S.toList . cnfClauses $ cnf')

updateV :: VM.MVector s a -> Int -> (a -> ST s (x, a)) -> ST s x
updateV m i fn = VM.read m i >>= \a -> do
  (x, a') <- fn a
  x <$ VM.write m i a'

updateSTRef :: STRef s a -> (a -> ST s (Maybe (x, a))) -> ST s (Maybe x)
updateSTRef m fn = readSTRef m >>= \a -> do
  fn a >>= \case
    Just (x, a') -> Just x <$ writeSTRef m a'
    Nothing      -> return Nothing

updateSTRef' :: STRef s a -> (a -> ST s (x, a)) -> ST s x
updateSTRef' m fn = readSTRef m >>= \a -> do
  (x, a') <- fn a
  writeSTRef m a' $> x


-- updateFactsAndOptions
--   :: STRef s [Int] -> STRef s IS.IntSet -> Int -> Clause -> ST s ()
-- updateFactsAndOptions factsRef optionsRef i (LS.splitLiterals -> (falses, trues))
--   = do
--     case IS.minView trues of
--       Nothing -> error $ "CNF is not IPF, no true variables in clause"
--       Just (x, v) | IS.null v && IS.null falses -> addFact x
--                   | IS.null falses              -> addOption
--                   | otherwise                   -> return ()
--  where
--   addFact x = modifySTRef factsRef (x :)
--   addOption = modifySTRef optionsRef (IS.insert i)
--
-- initializePropergation
--   :: Int -> V.Vector Clause -> (V.Vector [Int], ([Int], IS.IntSet))
-- initializePropergation numVars cnf = runST $ do
--   clauseLookup <- VM.replicate numVars IS.empty
--   factsRef     <- newSTRef []
--   optionsRef   <- newSTRef IS.empty
--
--   iforM_
--     cnf
--     \i c -> do
--       updateFactsAndOptions factsRef optionsRef i c
--       forM_ (IS.toList $ LS.variables c) (VM.modify clauseLookup (IS.insert i))
--
--   (,)
--     <$> (V.map (IS.toList) <$> V.freeze clauseLookup)
--     <*> ((,) <$> readSTRef factsRef <*> readSTRef optionsRef)
--
--
-- propergateToSatisfy
--   :: VM.MVector s Bool
--   -> VM.MVector s (Maybe Clause)
--   -> V.Vector [Int]
--   -> ([Int], IS.IntSet)
--   -> ST s IS.IntSet
-- propergateToSatisfy visited clauses clauseLookup (facts, options) = do
--   factsRef   <- newSTRef facts
--   optionsRef <- newSTRef options
--
--   let
--     nextFact      = MaybeT $ updateSTRef factsRef (return . uncons)
--
--     nextOptionVar = MaybeT $ updateSTRef'
--       optionsRef
--       \s -> do
--         (partitionEithers -> (rm, itms)) <- forM (IS.toList s) $ \i -> do
--           (firstVar <$> VM.read clauses i) <&> \case
--             Just v  -> Right v
--             Nothing -> Left i
--         return
--           ( maybe Nothing (Just . minimum) (NE.nonEmpty itms)
--           , IS.difference s (IS.fromList rm)
--           )
--       where firstVar mc = mc >>= fmap fst . IS.minView . LS.variables
--
--     propergate a = forM_
--       (clauseLookup V.! a)
--       \cidx -> do
--         updateV
--           clauses
--           cidx
--           \case
--             Just (LS.conditionClause (tt a) -> mclause) -> do
--               (, mclause)
--                 <$> traverse_ (updateFactsAndOptions factsRef optionsRef cidx)
--                               mclause
--             Nothing -> return ((), Nothing)
--
--     minimize vs = runMaybeT (nextFact <|> nextOptionVar) >>= \case
--       Just a -> updateV visited a (return . (, True)) >>= \case
--         True  -> minimize vs
--         False -> do
--           propergate a
--           minimize (IS.insert a vs)
--       Nothing -> return vs
--
--   minimize IS.empty
--
-- minimizeCNF :: Int -> V.Vector Clause -> IS.IntSet
-- minimizeCNF numVars cnf =
--   let (cl, x) = (initializePropergation numVars cnf)
--   in  minimizeCNF' cl x numVars cnf
--
-- minimizeCNF'
--   :: V.Vector [Int] -> ([Int], IS.IntSet) -> Int -> V.Vector Clause -> IS.IntSet
-- minimizeCNF' cl x numVars cnf = runST $ do
--   visited <- VM.replicate numVars False
--   clauses <- V.thaw (V.map Just cnf)
--   propergateToSatisfy visited clauses cl x

-- progression :: Int -> V.Vector Clause -> NE.NonEmpty IS.IntSet
-- progression numVars cnf = runST $ do
--   clauses <- V.thaw (V.map Just cnf)
--   visited <- VM.replicate numVars False
--
--   let (cl, x) = initializePropergation numVars cnf
--
--       findNextUnvisited i
--         | i < VM.length visited = VM.read visited i >>= \case
--           True  -> findNextUnvisited (i + 1)
--           False -> return $ Just i
--         | otherwise = return $ Nothing
--
--       progress = findNextUnvisited >=> \case
--         Just i' -> do
--           p <- propergateToSatisfy visited clauses cl ([i'], IS.empty)
--           (p :) <$> progress (i' + 1)
--         Nothing -> return []
--
--   (NE.:|) <$> propergateToSatisfy visited clauses cl x <*> progress 0

-- weightedProgression
--   :: (IS.IntSet -> Double) -> IPF -> IS.IntSet -> NE.NonEmpty IS.IntSet
-- weightedProgression cost (IPF cnf facts) vars =
--   (\(a NE.:| x) -> a <> facts NE.:| x) . fmap unmap $ progression
--     (V.length back)
--     (V.fromList . S.toList . cnfClauses $ cnf')
--  where
--   unmap        = foldMap (\i -> back V.! i) . IS.toList
--   (cnf', back) = compressCNF (vars `IS.difference` facts) cost cnf

parsePretty
  :: Parsec Void LazyText.Text a -> String -> LazyText.Text -> Either String a
parsePretty parser name bs = first errorBundlePretty $ parse parser name bs

readCNF :: LazyText.Text -> Either String (CNF, V.Vector Text.Text)
readCNF = parsePretty (cnfP <* eof) "cnf"

readCNFFromFile :: FilePath -> IO (CNF, V.Vector Text.Text)
readCNFFromFile fp = do
  a <- parsePretty (cnfP <* eof) fp <$> LazyText.readFile fp
  either fail return a

cnfP :: Parsec Void LazyText.Text (CNF, V.Vector Text.Text)
cnfP = do
  xs <- implicationP `sepEndBy` newline
  let vars  = setOf (folded . both . folded) xs
      there = M.fromList . map swap . V.toList $ V.indexed back
      back  = V.fromList (S.toList vars)
  return
    ( CNF
      ( S.fromList
      . map
          (fromJust . LS.joinLiterals . over both
                                             (IS.fromList . map (there M.!))
          )
      $ xs
      )
    , back
    )

 where
  itemP =
    LazyText.toStrict <$> takeWhileP Nothing (\a -> (' ' /= a /\ '\n' /= a))

  truesP = label "trues"
    $ choice [chunk "true" $> [], itemP `sepBy1` (chunk " /\\ ")]

  falsesP = label "falses"
    $ choice [chunk "false" $> [], itemP `sepBy1` (chunk " \\/ ")]

  implicationP = do
    as <- truesP
    void (chunk " ==> ")
    bs <- falsesP
    return (as, bs)



