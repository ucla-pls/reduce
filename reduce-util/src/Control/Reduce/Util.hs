{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators           #-}
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
  ( listReduction
  , runReduction

  , PredicateOptions (..)
  , ReductionOptions (..)

  , ReducerName (..)
  , ReductionException (..)

  , module Control.Reduce.Util.CliPredicate
  ) where

-- unliftio
import           UnliftIO
import           UnliftIO.Directory

-- containers
import qualified Data.IntSet                      as IS

-- vector
import qualified Data.Vector                      as V

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
import           Control.Applicative
import           Data.Maybe
import           Text.Printf

-- free
import           Control.Monad.Free.Church

-- reduce-util
import           Control.Reduce.Util.CliPredicate
import qualified Control.Reduce.Util.Logger       as L
import           Control.Reduce.Util.Metric
import           Control.Reduce.Problem

-- reduce
import           Control.Reduce


-- checkOutputPreserved ::
--   PredicateOptions
--   -> CmdOutput
--   -> (Maybe CmdOutput)
--   -> Bool
-- checkOutputPreserved PredicateOptions {..} (CmdOutput c oh eh) = \case
--   Just (CmdOutput{..}) ->
--     all id
--     [ not predOptPreserveExitCode || outputCode == c
--     , not predOptPreserveStdout || outputOut == oh
--     , not predOptPreserveStderr || outputErr == eh
--     ]
--   Nothing ->
--     False

-- checkCliPredicate :: FilePath -> CliPredicate a -> a -> L.Logger (CmdResult (Maybe CmdOutput), Bool)
-- checkCliPredicate fp CliPredicate {..} a = do
--   res <- runCommand fp command a
--   return (res, checkOutputPreserved predOpt expected (resultOutput res))


-- -- | A `CliPredicate` is a command that knows it's expected output.
-- data CliPredicate a = CliPredicate
--   { predOpt  :: PredicateOptions
--   , expected :: !CmdOutput
--   , command  :: !(Command a (Maybe CmdOutput))
--   }

-- -- | Calculate a baseline to try to emulate when reducing
-- baseline ::
--   FilePath
--   -> Command a (Maybe CmdOutput)
--   -> a
--   -> L.Logger (Maybe CmdOutput)
-- baseline workdir cmd a =
--   L.phase "Calculating baseline" $ do
--     resultOutput <$> runCommand workdir cmd a

-- genCliPredicate ::
--   PredicateOptions
--   -> FilePath
--   -> Command a (Maybe CmdOutput)
--   -> a
--   -> L.Logger (Maybe (CliPredicate a))
-- genCliPredicate predOpts workDir cmd a = do
--   i <- baseline workDir cmd a
--   return ((\i' -> CliPredicate predOpts i' cmd) <$> i)


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

-- newtype Problem r s a = Problem { runProblem :: (s -> (a, r), s) }

-- problem :: (s -> a) -> s -> Problem (MList '[]) s a
-- problem f s = Problem ((,MNil) . f, s)

-- -- | Get an indexed list of elements, this enables us to differentiate between stuff.
-- toIndexed :: Problem r [a] b -> Problem r [Int] b
-- toIndexed (Problem (ds, as)) =
--   Problem (ds . displ, indecies)
--   where
--    items = V.fromList as
--    indecies = V.toList . V.map fst . V.indexed $ items
--    displ ids =
--      catMaybes . V.toList $ V.map (const Nothing) items V.// [(i, items V.!? i) | i <- ids]

-- toStringified ::  Problem (MList r) String b -> Problem (MList (Stringify ': r)) [Int] b
-- toStringified (Problem (ds, as)) =
--   stringify (items V.!) $ Problem (ds . displ, indecies)
--   where
--    items = V.fromList as
--    indecies = V.toList . V.map fst . V.indexed $ items
--    displ ids =
--      catMaybes . V.toList $ V.map (const Nothing) items V.// [(i, items V.!? i) | i <- ids]

-- -- | Represent the inputs as a list of sets
-- toSetted :: Problem r [a] b -> Problem r [IS.IntSet] b
-- toSetted as =
--   let Problem (ds, indices) = toIndexed as
--   in Problem (ds . IS.toList . IS.unions, fmap IS.singleton indices)

-- count :: Problem (MList r) [a] b -> Problem (MList (Count ': r)) [a] b
-- count (Problem (fs, s')) =
--   Problem ((\s -> let (b, r) = fs s in (b, MCons (counted s) r)), s')

-- stringify :: (Int -> Char) -> Problem (MList r) [Int] b -> Problem (MList (Stringify ': r)) [Int] b
-- stringify itc (Problem (fs, s')) =
--   Problem ((\s -> let (b, r) = fs s in (b, MCons (stringified itc (Prelude.length s') s) r)), s')

listReduction :: ReducerName -> Reduction [x]
listReduction red xs =
  case red of
    Ddmin  -> ddmin predc xs
    Binary -> binaryReduction predc xs
    Linear -> linearReduction predc xs
  where
    predc = PredicateM check

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
    liftIO . BLC.writeFile "metrics.csv" $ headerString p
    (ee, (_, m)) <-
      runStateT
      (runExceptT . iterM (reduce start) $ reduction initial)
      (0, Nothing)
    return . fmap (fst . store) $ case ee of
      Left e  ->
        (Just e, fromMaybe initial m)
      Right a ->
        fromMaybe (Just ReductionFailed, initial) $ (Nothing,) <$> (a <|> m)
  where
    reduce start (Check a f) = do
      iteration <- gets fst
      now <- liftIO $ getCurrentTime

      let
        diff = now `diffUTCTime` start
        fp = printf "%04d" iteration

      when (0 < redOptTotalTimeout && redOptTotalTimeout < realToFrac diff) $
        throwError $ ReductionTimedOut

      when (0 < redOptMaxIterations && redOptMaxIterations < iteration) $
        throwError $ ReductionIterationsExceeded

      (res, success) <- lift . lift $ checkSolution p fp a

      liftIO . BLC.appendFile "metrics.csv" $
        metricRowString p (MetricRow a diff fp res success)

      when success $ do
        modify (\(i, _) -> (i, Just a))

      f success
