{-# LANGUAGE DataKinds           #-}
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

  , Reduction
  , runReduction

  , AbstractReduction (..)
  , runAbstractReduction


  , PredicateOptions (..)
  , ReductionOptions (..)

  , ReducerName (..)
  , ReductionException (..)

  , module Control.Reduce.Command
  , module Control.Reduce.Metric
  , module Control.Reduce.Problem
  ) where

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

-- free
import           Control.Monad.Free.Church

-- reduce-util
import           Control.Reduce.Command
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

type Reduction a = a -> ReductM a (Maybe a)

data AbstractReduction b = forall a.
  AbstractReduction (Reduction a) (Problem a b)

check :: a -> ReductM a Bool
check a = liftF $ Check a id

-- | Do a reduction over a list
listReduction :: ReducerName -> Reduction [x]
listReduction red xs =
  case red of
    Ddmin  -> ddmin predc xs
    Binary -> binaryReduction predc xs
    Linear -> linearReduction predc xs
  where
    predc = PredicateM check

-- | Do a reduction over a list of sets
setReduction :: Ord x => ReducerName -> Reduction [S.Set x]
setReduction red xs =
  case red of
    Ddmin  -> ddmin predc sxs
    Binary -> genericBinaryReduction (S.size . S.unions) predc xs
    Linear -> linearReduction predc sxs
  where
    sxs = L.sortOn (S.size) xs
    predc = PredicateM check

-- | Do a reduction over an 'IntSet'
intsetReduction :: ReducerName -> Reduction [IS.IntSet]
intsetReduction red xs =
  case red of
    Ddmin  -> ddmin predc sxs
    Binary -> genericBinaryReduction (IS.size . IS.unions) predc xs
    Linear -> linearReduction predc sxs
  where
    sxs = L.sortOn (IS.size) xs
    predc = PredicateM check


runAbstractReduction ::
  ReductionOptions
  -> FilePath
  -> AbstractReduction b
  -> L.Logger (Maybe ReductionException, b)
runAbstractReduction opts fp (AbstractReduction red problem) =
  runReduction opts fp red problem

runReduction ::
  ReductionOptions
  -> FilePath
  -> Reduction a
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
