{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-|
Module      : Control.Reduce.Util
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module provides utils, so that it is easier to write command-line reducers.
-}
module Control.Reduce.Util
  ( listReduction
  , intsetReduction
  , setReduction

  , TreeStrategy (..)
  , treeStrategy
  , hddReduction
  , graphReduction


  , Strategy
  , runReduction

  , AbstractProblem (..)
  , runAbstractProblem


  , PredicateOptions (..)
  , ReductionOptions (..)

  , ReducerName (..)
  , ReductionException (..)

  , module Control.Reduce.Command
  , module Control.Reduce.Metric
  , module Control.Reduce.Problem
  ) where

import Debug.Trace


-- unliftio
import           UnliftIO
import           UnliftIO.Directory

-- time
import           Data.Time

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BLC

-- mtl
import           Control.Monad.Except
import           Control.Monad.State

-- containers
import qualified Data.IntSet                as IS
import qualified Data.Set                   as S

-- base
import           Text.Printf
import qualified Data.List as L

-- vector
import qualified Data.Vector as V

-- free
import           Control.Monad.Free.Church

-- reduce-util
import           Control.Reduce.Command
import           Control.Reduce.Graph
import           Control.Reduce.Metric
import           Control.Reduce.Problem
import qualified Control.Reduce.Util.Logger as L

-- reduce
import           Control.Reduce

-- | The name of the reducer
data ReducerName
  = Ddmin
  | Linear
  | Binary
  deriving (Show, Eq)

data ReductionOptions = ReductionOptions
  { redOptTotalTimeout  :: !Double
  , redOptMaxIterations :: !Int
  , redOptKeepFolders   :: !Bool
  } deriving (Show, Eq)

data ReductionException
  = ReductionTimedOut
  | ReductionIterationsExceeded
  | ReductionFailed
  deriving (Show)

instance Exception ReductionException

data ReductF a f
  = Check a (Bool -> f)
  deriving (Functor)

type ReductM x = F (ReductF x)

type Strategy a = a -> ReductM a (Maybe a)

data AbstractProblem b = forall a.
  AbstractProblem (Strategy a) (Problem a b)

check :: a -> ReductM a Bool
check a = liftF $ Check a id



-- | Do a reduction over a list
listReductM :: ([x] -> a) -> ReducerName -> [x] -> ReductM a (Maybe a)
listReductM c name lst =
  fmap c <$> case name of
    Ddmin  -> ddmin predc lst
    Binary -> binaryReduction predc lst
    Linear -> linearReduction predc lst
  where
    predc = PredicateM $ check . c


listReduction :: ReducerName -> Strategy [a]
listReduction = listReductM id

-- | Do a reduction over a list of sets
setReduction :: Ord x => ReducerName -> Strategy [S.Set x]
setReduction red xs =
  case red of
    Ddmin  -> ddmin predc sxs
    Binary -> genericBinaryReduction (S.size . S.unions) predc xs
    Linear -> linearReduction predc sxs
  where
    sxs = L.sortOn (S.size) xs
    predc = PredicateM check

intsetReduction :: ReducerName -> Strategy [IS.IntSet]
intsetReduction = intsetReduct id

-- | Do a reduction over an 'IntSet'
intsetReduct :: ([IS.IntSet] -> a) -> ReducerName -> [IS.IntSet] -> ReductM a (Maybe a)
intsetReduct fn red xs =
  fmap fn <$> case red of
    Ddmin  -> ddmin predc sxs
    Binary -> genericBinaryReduction (IS.size . IS.unions) predc xs
    Linear -> linearReduction predc sxs
  where
    sxs = L.sortOn (IS.size) xs
    predc = PredicateM (check . fn)

-- | Strategy for reducing trees
data TreeStrategy
  = HddStrategy
  | GraphStrategy
  | FlatStrategy
  deriving (Show, Read, Ord, Eq)

treeStrategy ::
  TreeStrategy
  -> ReducerName
  -> Strategy [[Int]]
treeStrategy = \case
    FlatStrategy -> listReduction
    GraphStrategy -> graphReduction
    HddStrategy -> hddReduction

graphReduction :: ReducerName -> Strategy [[Int]]
graphReduction name items =
  let
    (graph, _) = buildGraphFromNodesAndEdges
        [ (i, i) | i <- items ]
        [ Edge a' rst () | a'@(_:rst) <- items ]
    labs = nodeLabels graph
    fn =
      fmap (labs V.!) . IS.toAscList . IS.unions
  in intsetReduct fn name (closures graph)

hddReduction :: ReducerName -> Strategy [[Int]]
hddReduction name = go 1
  where
    go :: Int -> Strategy [[Int]]
    go n items
      | length reductionLayer > 0 = do
        listReductM back name (traceShowId reductionLayer) >>= \case
          Just items' -> go (n+1) items'
          Nothing -> return $ Just items
     | otherwise =
       return $ Just items
        where
          (reductionLayer, rest) = L.partition (\i -> length i == n) items
          keep = S.fromList rest
          back = S.toAscList . S.union keep . S.fromList


runAbstractProblem ::
  ReductionOptions
  -> FilePath
  -> AbstractProblem b
  -> L.Logger (Maybe ReductionException, b)
runAbstractProblem opts fp (AbstractProblem red problem) =
  runReduction opts fp red problem

runReduction ::
  ReductionOptions
  -> FilePath
  -> Strategy a
  -> Problem a b
  -> L.Logger (Maybe ReductionException, b)
runReduction (ReductionOptions {..}) wf reduction p@(Problem {..}) = do
  start <- liftIO $ getCurrentTime
  createDirectory wf
  withCurrentDirectory wf $ do
    liftIO . BLC.writeFile "metrics.csv" $ headerString metric
    (ee, (_, m)) <-
      runStateT
      (runExceptT . iterM (reduce start) $ reduction initial)
      (0, initial)
    return . fmap store $ case ee of
      Left e  ->
        (Just e, m)
      Right a ->
        maybe (Just ReductionFailed, m) (Nothing,) a
  where
    reduce start (Check a f) = do
      iteration <- state (\(i, r) -> (i, (i + 1, r)))
      now <- liftIO $ getCurrentTime

      let
        diff = now `diffUTCTime` start
        fp = printf "%04d" iteration

      when (0 < redOptTotalTimeout && redOptTotalTimeout < realToFrac diff) $
        throwError $ ReductionTimedOut

      when (0 < redOptMaxIterations && redOptMaxIterations < iteration) $
        throwError $ ReductionIterationsExceeded

      L.info $ "Trying: " <> displayAnyMetric metric a

      (res, success) <- lift . lift  $ checkSolution p fp a

      L.info $ if success then "success" else "failure"

      liftIO . BLC.appendFile "metrics.csv" $
        metricRowString metric (MetricRow a diff fp res success)

      when success $ do
        modify (\(i, _) -> (i, a))

      f success
