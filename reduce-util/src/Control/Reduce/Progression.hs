{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-|
Module      : Control.Reduce.Progression
Copyright   : (c) Christian Gram Kalhauge, 2020
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module defines how to calculate a progression.
-}
module Control.Reduce.Progression
  ( calculateProgression
  , calculateLogicalClosure
  , calculateSimpleProgression

  , runProgression
  , progression
  , logicalClosure

  , generateBadOrder
  , generateGraphOrder
  , generateTotalGraphOrder
  , generateProgressionOrder
  )
where

-- base
import           Control.Monad.ST              as ST
import           Data.Either
import           Data.Functor
import qualified Data.List                     as L
import           Data.STRef                    as ST
import qualified Data.List.NonEmpty            as NE
import qualified Data.Set                      as S

-- lens
import           Control.Lens

-- mtl
import           Control.Monad.Reader

-- containers
import qualified Data.IntSet                   as IS

-- vector
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM

-- reduce-util
import           Control.Reduce.Graph          as G
import           Control.Reduce.Boolean.LiteralSet
                                               as LS
import           Control.Reduce.Boolean
import qualified Control.Reduce.Boolean.CNF    as CNF
import           Control.Reduce.Boolean.CNF     ( CNF(..) )

import Debug.Trace

-- | Calculate the progregssion
calculateProgression :: Order -> CNF -> IS.IntSet -> NE.NonEmpty IS.IntSet
calculateProgression order cnf vars =
  IS.fromList . fmap ((lookup V.!) . (lookup' V.!))
    <$> runProgression n cnf'' progression
 where
  (cnf', lookup) = CNF.limitCNF vars cnf
  n = V.length lookup
  lookup' = order n cnf'
  revlookup = inverseOrder lookup'
  cnf'' = CNF.vmapCNF (revlookup V.!) cnf'

-- | Calculate the progregssion, using the variable order
-- inheirit in the CNF.
calculateSimpleProgression ::
  CNF
  -> IS.IntSet
  -> NE.NonEmpty IS.IntSet
calculateSimpleProgression cnf vars =
  IS.fromList . fmap (lookup V.!)
    <$> runProgression n cnf' progression
 where
  (cnf', lookup) = CNF.limitCNF vars cnf
  n = V.length lookup


-- | Calculate the LogicalClosure
calculateLogicalClosure :: CNF -> IS.IntSet -> IS.IntSet
calculateLogicalClosure cnf vars =
  IS.fromList
    . fmap (lookup V.!)
    $ runProgression (V.length lookup) cnf' logicalClosure
 where
   (cnf', lookup) = CNF.limitCNF vars cnf

data ProgressionState s = ProgressionState
  { progClauses :: VM.MVector s (Maybe Clause)
  , progPositiveClauses :: STRef s IS.IntSet
  , progReverseLookup :: V.Vector [Int]
  , progVisited :: VM.MVector s Bool
  }

type ProgressionM s = ReaderT (ProgressionState s) (ST s)

runProgression :: Int -> CNF -> (forall s. ProgressionM s a) -> a
runProgression numVars (CNF clauses) runM = runST $ do
  let vClauses = V.fromList . S.toList $ clauses
  pClauses <- V.thaw $ V.map Just vClauses
  visited <- VM.replicate numVars False
  positive <- newSTRef IS.empty
  clauseLookup <- VM.replicate numVars IS.empty

  iforM_ vClauses \i c ->
    forM_ (IS.toList $ LS.variables c) do
      VM.modify clauseLookup (IS.insert i)

  cl <- V.freeze clauseLookup

  let
    initialState = ProgressionState
      { progClauses = pClauses
      , progPositiveClauses = positive
      , progReverseLookup = V.map IS.toList cl
      , progVisited = visited
      }

  flip runReaderT initialState do
    iforM_ vClauses \i c -> unless (hasNegativeClause c) do
      addPositiveClause i
    runM


progression :: ProgressionM s (NE.NonEmpty [Int])
progression = do
  a <- logicalClosure
  nv <- numberOfVariables
  (a NE.:|) <$> go 0 nv
 where
  go i nv
    | i < nv = isVisited i >>= \case
        True  -> go (i+1) nv
        False -> do
          conditionTrue i
          c <- logicalClosure
          ((c ++ [ i ]) :) <$> go (i+1) nv
    | otherwise = pure []

type Order = Int ->  CNF -> V.Vector Int

generateBadOrder :: Order
generateBadOrder n cnf = V.reverse $ generateGraphOrder n cnf

generateProgressionOrder :: Order
generateProgressionOrder n cnf = lookup
 where
  Just (_, cnf') = CNF.unitResolve cnf
  lookup = V.reverse . V.fromList . concat $
    runProgression n (CNF.transpose cnf') progression

generateGraphOrder :: Order
generateGraphOrder n cnf = lookup
 where
  Just (splitLiterals -> (_, tt), cnf') = CNF.unitResolve cnf

  -- Make this better, choose free variables first.
  (graph, _) = G.buildGraphFromNodesAndEdges
    [ (a, a) | a <- [ 0..n-1 ]]
    [ G.Edge () t f | (f, t) <- CNF.cnfDependencies cnf']

  lookup = V.fromList
    . (IS.toList tt ++)
    . reverse
    . filter (Prelude.not . (`IS.member` tt)) $ G.postOrd graph

generateTotalGraphOrder :: Order
generateTotalGraphOrder n cnf = lookup
 where
  Just (splitLiterals -> (_, tt), cnf') = CNF.unitResolve cnf

  -- Make this better, choose free variables first.
  (graph, _) = G.buildGraphFromNodesAndEdges
    [ (a, a) | a <- [ 0..n-1 ]]
    [ G.Edge () t f
    | c <- S.toList $ CNF.cnfClauses cnf'
    , let (ff, tt) = splitLiterals c
    , (f, t) <- liftM2 (,) (IS.toList ff) (IS.toList tt)
    ]

  lookup = V.fromList
    . (IS.toList tt ++)
    . reverse
    . filter (Prelude.not . (`IS.member` tt)) $ G.postOrd graph

inverseOrder :: V.Vector Int -> V.Vector Int
inverseOrder lookup = V.create do
  v <- VM.new (V.length lookup)
  iforM_ lookup (flip $ VM.write v)
  return v

numberOfVariables :: ProgressionM s Int
numberOfVariables = ReaderT $ pure . VM.length . progVisited

isVisited :: Int -> ProgressionM s Bool
isVisited i = ReaderT $ \p -> VM.read (progVisited p) i

markVisited :: Int -> ProgressionM s ()
markVisited i = ReaderT $ \p -> VM.write (progVisited p) i True

-- | The post-order logical closure visits the variables
-- in a set of clauses in post-order.
logicalClosure :: ProgressionM s [Int]
logicalClosure = go where
  go = nextPositiveVar >>= \case
    Just v -> do
      conditionTrue v
      (v:) <$> go
    Nothing ->
      return []

conditionTrue :: Int -> ProgressionM s ()
conditionTrue v = do
  mapM_ conditionClause =<< clausesOf v
  markVisited v
 where
  conditionClause :: Int -> ProgressionM s ()
  conditionClause cidx = updateClause cidx \case
    Just (LS.conditionClause (tt v) -> Just clause) -> do
      unless (hasNegativeClause clause) $ addPositiveClause cidx
      return ((), Just clause)
    _ -> return ((), Nothing)

addPositiveClause :: Int -> ProgressionM s ()
addPositiveClause cidx = ReaderT \p ->
  ST.modifySTRef' (progPositiveClauses p) (IS.insert cidx)

updateClause ::
  Int
  -> (Maybe Clause -> ProgressionM s (a, Maybe Clause))
  -> ProgressionM s a
updateClause i fn = ReaderT \p -> do
  x <- VM.read (progClauses p) i
  (a, st) <- runReaderT (fn x) p
  VM.write (progClauses p) i st
  return a

clausesOf :: Int -> ProgressionM s [Int]
clausesOf v = ReaderT \p -> pure $ progReverseLookup p V.! v

nextPositiveVar :: ProgressionM s (Maybe Int)
nextPositiveVar = ReaderT \ProgressionState { .. } -> updateSTRef' progPositiveClauses \s -> do
  (partitionEithers -> (rm, itms)) <- forM (IS.toList s) $ \i ->
    (firstVar <$> VM.read progClauses i) <&> \case
      Just v  -> Right v
      Nothing -> Left i
  return
    ( Just . minimum =<< NE.nonEmpty itms
    , IS.difference s (IS.fromList rm)
    )
 where
  firstVar mc = mc >>= fmap fst . IS.minView . LS.variables

-- updateV :: VM.MVector s a -> Int -> (a -> ST s (x, a)) -> ST s x
-- updateV m i fn = VM.read m i >>= \a -> do
--   (x, a') <- fn a
--   x <$ VM.write m i a'
--
-- updateSTRef :: STRef s a -> (a -> ST s (Maybe (x, a))) -> ST s (Maybe x)
-- updateSTRef m fn = readSTRef m >>= \a -> do
--   fn a >>= \case
--     Just (x, a') -> Just x <$ writeSTRef m a'
--     Nothing      -> return Nothing

updateSTRef' :: STRef s a -> (a -> ST s (x, a)) -> ST s x
updateSTRef' m fn = readSTRef m >>= \a -> do
  (x, a') <- fn a
  writeSTRef m a' $> x


-- -- | Implicative Positive Form,
-- -- The requirements is that all clauses contain at least one possitive
-- -- varaible.
-- -- All singleton positive variables are split from the set.
-- data IPF = IPF
--   { ipfClauses :: CNF
--   , ipfFacts   :: IS.IntSet
--   } deriving (Show)
--
-- debugIpf :: IPF -> IO ()
-- debugIpf = debugIpfWith shows
--
-- debugIpfWith :: (Int -> ShowS) -> IPF -> IO ()
-- debugIpfWith fn (IPF cnf facts) = do
--   putStrLn $ "Facts: " ++ showListWith fn (IS.toList facts) ""
--   forM_ (cnfClauses cnf) \a -> putStrLn (LS.displayImplication fn a "")
--
--
--
-- fromCNF :: CNF -> Maybe IPF
-- fromCNF cnf
--   | any (IS.null . snd . LS.splitLiterals) $ cnfClauses cnf = Nothing
--   | otherwise = unitResolve cnf
--   <&> \(LS.splitLiterals -> (_, trues), cnf') -> (IPF cnf' trues)
--
--
-- -- | Conditioning an IPF with positive literals produce a IPF.
-- conditionIPF :: IS.IntSet -> IPF -> IPF
-- conditionIPF is (IPF cnf facts) = if IS.null falses
--   then IPF cnf' (facts `IS.union` trues)
--   else error $ "unexpected: " ++ show is ++ " " ++ show falses
--  where
--     -- unit propergation on ipfs are safe
--   (LS.splitLiterals . fromJust -> (falses, trues), cnf') =
--     unitPropergation (LS.joinLiterals' (IS.empty, is)) cnf
--
-- -- -- | Given a set of positive varaibles that is a true assignent to the
-- -- -- problem we can create an IPF that is limted to only those variables.
-- -- limitIPF' :: IS.IntSet -> IPF -> IPF
-- -- limitIPF' is (IPF cnf facts) =
-- --   IPF
-- --     (CNF . S.fromList . mapMaybe (LS.limitClause is) . S.toList $ cnfClauses cnf)
-- --     (is `IS.union` facts)
-- --     facts
-- --
-- -- limitIPF'' :: IS.IntSet -> IPF -> Maybe IPF
-- -- limitIPF'' is (IPF cnf _ facts) =
-- --   let cnf' = CNF . S.fromList . mapMaybe (LS.limitClause is) . S.toList $ cnfClauses cnf
-- --   in removeSingletons cnf' <&> \(t, cnf'') ->
-- --     IPF cnf''
-- --     (is `IS.union` facts)
-- --     (facts `IS.union` snd (LS.splitLiterals t))
--
-- learnClauseIPF :: IS.IntSet -> IPF -> IPF
-- learnClauseIPF is ipf
--   | IS.size is == 0 = error "can't learn an empty clause"
--   | IS.size is == 1 = conditionIPF is ipf
--   | otherwise = ipf
--     { ipfClauses =
--       CNF
--         ( S.insert (LS.joinLiterals' (IS.empty, is))
--         $ cnfClauses (ipfClauses ipf)
--         )
--     }
--
-- -- fastLWCC ::
-- --   (IS.IntSet -> Double)
-- --   -> IPF
-- --   -> IS.IntSet
-- --   -> IS.IntSet
-- -- fastLWCC cost ipf input =
-- --   facts `IS.union` unmap (minimizeCNF (V.length back) (V.fromList . S.toList . cnfClauses $ cnf'))
-- --  where
-- --   (IPF cnf facts) = conditionIPF input ipf
-- --   unmap = foldMap (\i -> back V.! i) . IS.toList
-- --   (cnf', back) = compressCNF (vars `IS.difference` facts) cost cnf
-- --
--
-- -- -- | Calculate the Logical Closure from an set.
-- -- logicalClosure ::
-- --   IPF
-- --   -- ^ the ipf
-- --   -> IS.IntSet
-- --   -- ^ valid variables
-- --   -> IS.IntSet
-- --   -- ^ input set
-- --   -> IS.IntSet
-- -- logicalClosure (IPF cnf facts) vars = \input -> IS.unions
-- --   [ input
-- --   , facts
-- --   , let
-- --       requiredItems = mapMaybe (there IM.!?) $ IS.toList input
-- --       closure = minimizeCNF'
-- --         lookupC
-- --         (facts' ++ requiredItems, options')
-- --         (V.length back)
-- --         clauses
-- --     in unmap closure
-- --   ]
-- --
-- --  where
-- --   unmap =
-- --     foldMap (\i -> IS.singleton $ back V.! i) . IS.toList
-- --
-- --   (cnf', there, back) =
-- --     shrinkCNF (vars `IS.difference` facts) cnf
-- --
-- --   (lookupC, (facts', options')) =
-- --     initializePropergation (V.length back) clauses
-- --
-- --   clauses =
-- --     (V.fromList . S.toList . cnfClauses $ cnf')
--


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

