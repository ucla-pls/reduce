{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-|
Module      : Control.Reduce.Util
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module provides utils, so that it is easier to write command-line reducers.

* TODO

- Improve interface for inputs to the CmdOptions
- Add support for file-trees (folders)
- Add support for JSON

* Ideas

Different types of inputs.

Lists, Trees, Graphs.


* Notes:

Things goes from

 IOItem
  |  ^
  v  |
UserItem
  |  ^
  v  |
ListItem


-}
module Control.Reduce.Util
  (
    baseline

  , listReduction
  , runReduction

  , CliPredicate (..)

  , PredicateOptions (..)
  , ReductionOptions (..)
  -- , toPredicateM
  -- , ReductionProblem (..)
  -- , specializeReduction
  -- , mkReductionProblem

  -- , ReductionModel (..)
  -- , applyModel
  -- , model

  -- , Metric (..)
  -- , counted
  -- , displayed
  -- , Count (..)
  -- , Display(..)

  , ReducerName (..)

  , ReductionException (..)
  -- , reduce
  -- , setreduce

  , module Control.Reduce.Util.CliPredicate
  ) where

-- unliftio
import           UnliftIO
import           UnliftIO.Directory

-- -- containers
-- import qualified Data.IntSet                           as IS

-- -- vector
-- import qualified Data.Vector                           as V

-- cassava
import qualified Data.Csv                         as C

-- time
import           Data.Time

-- bytestring
import qualified Data.ByteString.Lazy.Char8       as BLC

-- mtl
import           Control.Monad.Except
import           Control.Monad.State

-- base
import           Text.Printf
import           Data.Maybe
import           Control.Applicative

-- free
import           Control.Monad.Free.Church

-- reduce-util
import           Control.Reduce.Util.CliPredicate
import qualified Control.Reduce.Util.Logger       as L
import           Control.Reduce.Util.Metric

-- reduce
import           Control.Reduce

-- | Predicate options, defines which elements to check
data PredicateOptions = PredicateOptions
  { predOptPreserveExitCode :: !Bool
  , predOptPreserveStdout   :: !Bool
  , predOptPreserveStderr   :: !Bool
  } deriving (Show, Eq)

checkOutputPreserved ::
  PredicateOptions
  -> CmdOutput
  -> (Maybe CmdOutput)
  -> Bool
checkOutputPreserved PredicateOptions {..} (CmdOutput c oh eh) = \case
  Just (CmdOutput{..}) ->
    all id
    [ not predOptPreserveExitCode || outputCode == c
    , not predOptPreserveStdout || outputOut == oh
    , not predOptPreserveStderr || outputErr == eh
    ]
  Nothing ->
    False

checkCliPredicate :: FilePath -> CliPredicate a -> a -> L.Logger (CmdResult (Maybe CmdOutput), Bool)
checkCliPredicate fp CliPredicate {..} a = do
  res <- runCommand fp command a
  return (res, checkOutputPreserved predOpt expected (resultOutput res))


-- | A `CliPredicate` is a command that knows it's expected output.
data CliPredicate a = CliPredicate
  { predOpt  :: PredicateOptions
  , expected :: !CmdOutput
  , command  :: !(Command a (Maybe CmdOutput))
  }

-- | Calculate a baseline to try to emulate when reducing
baseline ::
  FilePath
  -> Command a (Maybe CmdOutput)
  -> a
  -> L.Logger (Maybe CmdOutput)
baseline workdir cmd a =
  L.phase "Calculating baseline" $ do
    resultOutput <$> runCommand workdir cmd a


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

check :: a -> ReductM a Bool
check a = liftF $ Check a id

listReduction :: ReducerName -> Reduction [x]
listReduction red xs =
  case red of
    Ddmin  -> ddmin predc xs
    Binary -> binaryReduction predc xs
    Linear -> linearReduction predc xs
  where
    predc = PredicateM check

runReduction ::
  forall a b c.
  (Metric c)
  => ReductionOptions
  -> FilePath
  -> ((a, b) -> c)
  -> (a -> b)
  -> CliPredicate b
  -> Reduction a
  -> a
  -> L.Logger (Maybe ReductionException, b)
runReduction (ReductionOptions {..}) wf metric fab predc reduction initial = do
  start <- liftIO $ getCurrentTime
  createDirectory wf
  withCurrentDirectory wf $ do
    liftIO . BLC.writeFile "metrics.csv"
      $ C.encodeDefaultOrderedByNameWith C.defaultEncodeOptions ([] :: [MetricRow c])
    (ee, (_, m)) <- runStateT (runExceptT (iterM (reduce start) (reduction initial))) (0, Nothing)
    return . fmap fab $ case ee of
      Left e  ->
        (Just e, fromMaybe initial m)
      Right a ->
        fromMaybe (Just ReductionFailed, initial) $ (Nothing,) <$> (a <|> m)

  where
    reduce ::
      UTCTime
      -> ReductF a (ExceptT ReductionException (StateT (Int, Maybe a) L.Logger) (Maybe a))
      -> ExceptT ReductionException (StateT (Int, Maybe a) L.Logger) (Maybe a)
    reduce start (Check a f) = do

      iteration <- gets fst
      now <- liftIO $ getCurrentTime

      let
        diff = now `diffUTCTime` start
        fp = printf "%04d" iteration
        b = fab a
        m = metric (a, b)

      when (0 < redOptTotalTimeout && redOptTotalTimeout < realToFrac diff) $
        throwError $ ReductionTimedOut

      when (0 < redOptMaxIterations && redOptMaxIterations < iteration) $
        throwError $ ReductionIterationsExceeded

      L.info $ "Trying: " <> displayMetric m
      (res, success) <- lift . lift $ checkCliPredicate fp predc b

      L.info $ if success then "success" else "failure"

      logMetric (MetricRow m diff fp res success)

      when success $ do
        modify (\(i, _) -> (i, Just a))

      f success

    logMetric mr = do
      liftIO
        . BLC.appendFile "metrics.csv"
        $ C.encodeDefaultOrderedByNameWith
          ( C.defaultEncodeOptions { C.encIncludeHeader = False } )
          [ mr ]
