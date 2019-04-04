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
{-# LANGUAGE TupleSections             #-}
{-|
Module      : Control.Reduce.Problem
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module defines a reduction problem.
-}

module Control.Reduce.Problem where

-- vector
import qualified Data.Vector                as V

-- lens
import           Control.Lens

-- base
import           Control.Monad
import           Data.Functor
import           Data.Maybe
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup


-- containers
import qualified Data.IntSet                as IS
import qualified Data.Set                   as S

-- reduce-util
import           Control.Reduce.Command
import           Control.Reduce.Reduction
import           Control.Reduce.Graph
import           Control.Reduce.Metric
import qualified Control.Reduce.Util.Logger as L

-- | A Problem is something that we can reduce, by running a program on with the
-- correct inputs.
data Problem s a = Problem
  { initial     :: !s
  , store       :: s -> a
  , metric      :: AnyMetric s
  , expectation :: !Expectation
  , command     :: !(Command a (Maybe CmdOutput))
  }

-- | Update the problem. This is useful if some partial reduction was achieved.
updateProblem :: (s -> s) -> Problem s a -> Problem s a
updateProblem fs p@Problem{..} = p { initial = fs initial }

-- | Set the problem to a new begining value
resetProblem :: s -> Problem s a -> Problem s a
resetProblem s = updateProblem (const s)

-- | Lift the problem to a new domain. Requires that the new domain is
-- isomorphic to the original domain.
liftProblem :: (s -> t) -> (t -> s) -> Problem s a -> Problem t a
liftProblem st ts =
  refineProblem ((ts,) . st)

-- | Given the original definition of the problem, refine the problem.
refineProblem :: (s -> (t -> s, t)) -> Problem s a -> Problem t a
refineProblem f Problem {..} =
  Problem t (store . tf) (tf `contramap` metric) expectation command
  where
    (tf, t) = f initial

-- | Get an indexed list of elements, this enables us to differentiate between stuff.
toClosures :: Ord n => [Edge e n] -> Problem [n] b -> Problem [IS.IntSet] b
toClosures edges' = refineProblem refined
  where
    refined items = (fs, closures graph)
      where
        fs = map (nodeLabel . (nodes graph V.!)) . IS.toList . IS.unions
        (graph, _) = buildGraphFromNodesAndEdges (map (\a -> (a,a)) items) edges'

-- | Given a 'Reduction' from s given t, make the problem to reduce
-- a list of t's with indicies.
toReductionList :: Reduction s t -> Problem s b -> Problem (V.Vector (Maybe t)) b
toReductionList red =
  refineProblem $ \s ->
    ( flip (limiting red) s . (\v i -> maybe False (const True) $ v V.!? i)
    , V.map Just . V.fromList $ toListOf (subelements red) $ s
    )

-- | Get an inde
toReductionTree' :: Reduction s s -> Problem s b -> Problem (S.Set (NE.NonEmpty Int)) b
toReductionTree' red =
  refineProblem $ \s ->
    ( flip (limit $ treeReduction red) s . flip S.member
    , S.fromList $ indicesOf (treeReduction red) s
    )

toReductionTree :: Reduction s s -> Problem s b -> Problem [[Int]] b
toReductionTree red =
  liftProblem
  (fmap NE.toList . S.toAscList)
  (S.fromList . catMaybes . fmap NE.nonEmpty)
  . toReductionTree' red

-- | Get an indexed list of elements, this enables us to differentiate between stuff.
toIndexed :: Problem [a] b -> Problem [Int] b
toIndexed = toIndexed' . toFixedLenght

-- | Make the problem fixed length.
toFixedLenght :: Problem [a] b -> Problem (V.Vector (Maybe a)) b
toFixedLenght = liftProblem (V.map Just . V.fromList) (catMaybes . V.toList)

-- | Turn a problem of reducing maybe values to one of reducting a list of integers.
toIndexed' :: Problem (V.Vector (Maybe a)) b -> Problem [Int] b
toIndexed' Problem {..} =
  Problem indicies (store . displ) (displ `contramap` metric) expectation command
  where
   nothings = V.map (const Nothing) initial
   indicies = V.toList . V.map fst . V.indexed $ initial
   displ ids = nothings V.// [(i, join $ initial V.!? i) | i <- ids]

-- | Create a stringed version that also measures the cl
toStringified :: (a -> Char) -> Problem [a] b -> Problem [Int] b
toStringified fx =
  toIndexed'
  . meassure (Stringify . V.toList . V.map (maybe '·' fx))
  . toFixedLenght

-- | Add a metric to the problem
meassure :: Metric r => (s -> r) -> Problem s a -> Problem s a
meassure sr p =
  p { metric = addMetric sr . metric $ p }


-- | Setup the problem.
setupProblem ::
  PredicateOptions
  -> FilePath
  -> Command a (Maybe CmdOutput)
  -> a
  -> L.Logger (Maybe (Problem a a))
setupProblem opts workDir cmd a =
  L.phase "Calculating Initial Problem" $
  (resultOutput <$> runCommand workDir cmd a) >>= \case
  Just output ->
    return . Just
    $ Problem a id emptyMetric (zipExpectation opts output) cmd
  Nothing ->
    return Nothing


checkSolution ::
  Problem a b
  -> FilePath
  -> a
  -> L.Logger (CmdResult (Maybe CmdOutput), Bool)
checkSolution Problem{..} fp a = do
  -- L.info $ "Trying: " <> displayMetric m
  res <- runCommand fp command $ store a
  let success = checkExpectation expectation (resultOutput res)
  -- L.info $ if success then "success" else "failure"
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
  } deriving (Show, Eq)

instance Semigroup Expectation where
  Expectation a b c <> Expectation a' b' c' =
    Expectation (takeLast a a') (takeLast b b') (takeLast c c')
    where
      takeLast :: Maybe a -> Maybe a -> Maybe a
      takeLast = with Last getLast
      with _to _from x y = fmap _from (fmap _to x <> fmap _to y)

-- | Zip the PredicateOptions with a CmdOutput to build an Expectation
zipExpectation :: PredicateOptions -> CmdOutput -> Expectation
zipExpectation PredicateOptions{..} CmdOutput{..} =
  Expectation
  (guard predOptPreserveExitCode $> outputCode)
  (guard predOptPreserveStdout   $> outputOut)
  (guard predOptPreserveStderr   $> outputErr)

-- | Check if the output matches the expectation.
checkExpectation :: Expectation -> Maybe CmdOutput -> Bool
checkExpectation Expectation{..} = \case
  Just CmdOutput{..} ->
    maybe True (outputCode ==) expectedExitCode
    && maybe True (outputOut ==) expectedStdout
    && maybe True (outputErr ==) expectedStderr
  Nothing ->
    False
