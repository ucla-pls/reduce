{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-|
Module      : Control.Reduce.Problem
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module defines a reduction problem.
-}

module Control.Reduce.Problem where

-- cassava
import qualified Data.Csv                         as C

-- vector
import qualified Data.Vector                      as V

-- text
import qualified Data.Text.Lazy.Builder           as Builder

-- lens
import           Control.Lens

-- base
import           Control.Monad
import           Data.Maybe
import           Data.Functor

-- bytestring
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy             as BL

-- reduce-util
import           Control.Reduce.Util.CliPredicate
import qualified Control.Reduce.Util.Logger       as L
import           Control.Reduce.Util.Metric

-- | A Problem is something that we can reduce, by running a program on with the
-- correct inputs.
data Problem s a = forall r. Metric (MList r) =>  Problem
  { initial     :: !s
  , store       :: s -> (a, MList r)
  , expectation :: !Expectation
  , command     :: !(Command a (Maybe CmdOutput))
  }

-- | Update the problem. This is useful if some partial reduction was achieved.
updateProblem :: (s -> s) -> Problem s a -> Problem s a
updateProblem fs p@(Problem {..}) = p { initial = fs initial }

-- | Set the problem to a new begining value
resetProblem :: s -> Problem s a -> Problem s a
resetProblem s = updateProblem (const s)

-- | Lift the problem to a new domain. Requires that the new domain is
-- isomorphic to the original domain.
liftProblem :: (s -> t) -> (t -> s) -> Problem s a -> Problem t a
liftProblem st ts (Problem {..}) =
  Problem (st initial) (store . ts) expectation command


-- | Get an indexed list of elements, this enables us to differentiate between stufftoIndexed :: Problem r [a] b -> Problem r [Int] b
toIndexed Problem {..} =
  Problem indicies (store . displ) expectation command
  where
   items = V.fromList initial
   indicies = V.toList . V.map fst . V.indexed $ items
   displ ids =
     catMaybes . V.toList $ V.map (const Nothing) items V.// [(i, items V.!? i) | i <- ids]



addMetric :: Metric r => (s -> r)  -> Problem s a -> Problem s a
addMetric sr Problem {..} =
  Problem
  initial
  (\s -> let (a, rs) = store s in (a, MCons (sr s) rs))
  expectation
  command

headerString :: Problem s a -> BL.ByteString
headerString Problem {..} =
  case store of
    (a :: s -> (a, MList r)) ->
      C.encodeDefaultOrderedByNameWith
      C.defaultEncodeOptions ([] :: [MetricRow (MList r)])

metricRowString :: Problem s a -> MetricRow s -> BL.ByteString
metricRowString Problem {..} row =
  let (a, r) = store (metricRowContent row)
  in C.encodeDefaultOrderedByNameWith
     ( C.defaultEncodeOptions { C.encIncludeHeader = False } )
     [ row $> r]

-- | Setup the problem.
setupProblem ::
  PredicateOptions
  -> FilePath
  -> Command a (Maybe CmdOutput)
  -> a
  -> L.Logger (Maybe (Problem a a))
setupProblem opts workDir cmd a =
  L.phase "Calculating Initial Problem" $ do
    (resultOutput <$> runCommand workDir cmd a) >>= \case
      Just output ->
        return . Just
        $ Problem a (\a -> (a, MNil)) (zipExpectation opts output) cmd
      Nothing ->
        return Nothing


checkSolution ::
  Problem a b
  -> FilePath
  -> a
  -> L.Logger (CmdResult (Maybe CmdOutput), Bool)
checkSolution (Problem {..}) fp a = do
  let (b, m) = store a
  L.info $ "Trying: " <> displayMetric m
  res <- runCommand fp command b
  let success = checkExpectation expectation (resultOutput res)
  L.info $ if success then "success" else "failure"
  return (res, success)


-- * Expectation

-- | Predicate options, defines which elements to check
data PredicateOptions = PredicateOptions
  { predOptPreserveExitCode :: !Bool
  , predOptPreserveStdout   :: !Bool
  , predOptPreserveStderr   :: !Bool
  } deriving (Show, Eq)

-- | An `Expectation` can or cannot be meet by a `CmdOutput`
data Expectation = Expectation
  { expectedExitCode :: Maybe ExitCode
  , expectedStdout   :: Maybe Sha256
  , expectedStderr   :: Maybe Sha256
  } deriving (Show, Eq, Semigroup, Monoid)

-- | Zip the PredicateOptions with a CmdOutput to build an Expectation
zipExpectation :: PredicateOptions -> CmdOutput -> Expectation
zipExpectation (PredicateOptions {..}) (CmdOutput {..}) =
  Expectation
  (guard predOptPreserveExitCode $> outputCode)
  (guard predOptPreserveStdout   $> outputOut)
  (guard predOptPreserveStderr   $> outputErr)

-- | Check if the output matches the expectation.
checkExpectation :: Expectation -> (Maybe CmdOutput) -> Bool
checkExpectation (Expectation {..}) = \case
  Just (CmdOutput {..} ) ->
    all id
    [ maybe True (outputCode ==) expectedExitCode
    , maybe True (outputOut ==) expectedStdout
    , maybe True (outputErr ==) expectedStderr
    ]
  Nothing ->
    False
