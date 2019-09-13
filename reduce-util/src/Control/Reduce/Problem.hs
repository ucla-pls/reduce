{-# LANGUAGE ConstraintKinds           #-}
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
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-|
Module      : Control.Reduce.Problem
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module defines a reduction problem and how to run it.
-}

module Control.Reduce.Problem
  (

  -- * Problem
    Problem (..)

  -- ** Constructors
  , setupProblem
  , setupProblemFromFile
  , PredicateOptions (..)

  -- ** Run the problem
  , MonadReductor
  , runReductionProblem
  , checkSolution

  , ReductionOptions (..)
  , defaultReductionOptions

  -- ** Combinators
  , updateProblem
  , resetProblem
  , liftProblem

  -- ** Problem Refinements
  , toReductionList
  , toReductionTree

  , meassure

  -- * Expectation

  , Expectation (..)
  , checkExpectation
  , zipExpectation

  ) where

-- base
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Exception          (AsyncException (..))
import           Data.Functor
import           Data.Time
import           Text.Printf
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.Semigroup

-- unlifio
import           UnliftIO
import           UnliftIO.Directory

-- vector
import qualified Data.Vector                as V

-- lens
import           Control.Lens

-- mtl
import           Control.Monad.Reader.Class

-- filepath
import           System.FilePath

-- dirtree
import           System.DirTree

-- bytestring
import qualified Data.ByteString.Lazy       as BL

-- containers
import qualified Data.IntSet                as IS
import qualified Data.Set                   as S

-- mtl
import Control.Monad.Reader

-- reduce
import           Control.Reduce

-- reduce-util
import           Control.Reduce.Command
import           Control.Reduce.Graph
import           Control.Reduce.Metric
import           Control.Reduce.Reduction
import qualified Control.Reduce.Util.Logger as L

-- | The reduction options
data ReductionOptions = ReductionOptions
  { _redTotalTimelimit    :: !Double
  -- ^ The total (wall-clock) time in seconds to run the reduction problem
  -- (negative means infinite)
  , _redMaxIterations   :: !Int
  -- ^ The max number of invocations of the predicate (negative means infinite)
  , _redKeepFolders     :: !Bool
  -- ^ Whether to keep the folders or not
  , _redMetricsFile        :: !FilePath
  -- ^ An absolute path to the metrics files, or relative to the work-folder
  , _redPredicateTimelimit :: !Double
  -- ^ the timelimit of a single run of the predicate in seconds.
  } deriving (Show, Eq)

-- | The default reduction options
--
-- @
-- defaultReductionOptions = ReductionOptions
--   { _redTotalTimelimit = -1
--   , _redMaxIterations = -1
--   , _redKeepFolders = True
--   , _redMetricsFile = "metrics.csv"
--   , _redPredicateTimelimit = 60
--   }
-- @
defaultReductionOptions :: ReductionOptions
defaultReductionOptions = ReductionOptions
  { _redTotalTimelimit = -1
  , _redMaxIterations = -1
  , _redKeepFolders = True
  , _redMetricsFile = "metrics.csv"
  , _redPredicateTimelimit = 60
  }

makeClassy ''ReductionOptions

-- | Predicate options, defines which elements to check
data PredicateOptions = PredicateOptions
  { predOptPreserveExitCode :: !Bool
  , predOptPreserveStdout   :: !Bool
  , predOptPreserveStderr   :: !Bool
  } deriving (Show, Eq)

makeClassy ''PredicateOptions


-- | A short hand for a monad reductor
type MonadReductor env m =
  (HasReductionOptions env, L.HasLogger env, MonadReader env m, MonadUnliftIO m)

-- | `runReduction` is the main function of this package. It is a wrapper
-- around a strategy to make it work with a problem.
runReductionProblem ::
  forall a env m.
  MonadReductor env m
  => FilePath
  -- ^ The work directory
  -> Reducer IO a
  -- ^ The reducer to use on the problem
  -> Problem a
  -- ^ The `Problem` to reduce
  -> m (Maybe ReductionException, a)
runReductionProblem wf reducer p = do
  opts <- view reductionOptions
  start <- liftIO $ getCurrentTime
  env <- ask

  createDirectory wf
  withCurrentDirectory wf . liftIO $ do
    record <- setupMetric (metric p) (opts ^. redMetricsFile)
    iterRef <- newIORef 1
    succRef <- newIORef (initial p)

    ee <- goWith (initial p) $ \a -> flip runReaderT env $ do
      (fp, _diff) <- checkTimeouts start iterRef opts

      L.info $ "Trying (Iteration " <> L.displayString fp <> ")"
      L.debug $ displayAnyMetric (metric p) a

      (judgment, result) <- checkSolution fp p a

      L.info $ (L.displayString $ showJudgment judgment)

      liftIO $ record (MetricRow a _diff fp judgment result)

      let success = judgment == Success

      when success $ writeIORef succRef a
      return success

    m <- readIORef succRef
    return $ case ee of
      Left e ->
        (Just e, m)
      Right a ->
        maybe (Just ReductionFailed, m) (Nothing,) a
  where
    goWith a f = liftIO $ do
      catches (Right <$> reducer f a)
        [ Handler $ \case
            UserInterrupt -> return $ Left ReductionInterupted
            x -> throwIO x
        , Handler (return . Left)
        ]

    checkTimeouts start iterRef ReductionOptions{..} = do
      now <- liftIO $ getCurrentTime
      let _diff = now `diffUTCTime` start
      when (0 < _redTotalTimelimit && _redTotalTimelimit < realToFrac _diff) $
        throwIO $ ReductionTimedOut

      iteration <- atomicModifyIORef iterRef (\i -> (i + 1, i))
      when (0 < _redMaxIterations && _redMaxIterations < iteration) $
        throwIO $ ReductionIterationsExceeded

      return (printf "%04d" iteration, _diff)

-- | A Reduction Exception
data ReductionException
  = ReductionTimedOut
  -- ^ The reduction timed out.
  | ReductionIterationsExceeded
  -- ^ The number of iterations was exceeded.
  | ReductionFailed
  -- ^ The reduction failed to find a reduction.
  | ReductionInterupted
  -- ^ The reduction was interrupted.
  deriving (Show, Eq, Ord)

instance Exception ReductionException


-- | Given a folder to run in, check that a problem has been solved with `a`.
-- this function should be run in some workfolder
checkSolution ::
  MonadReductor env m
  => FilePath
  -> Problem a
  -> a
  -> m (Judgment, Maybe (CmdResult (Maybe CmdOutputSummary)))
checkSolution fp Problem{..} a = do
  timelimit <- view redPredicateTimelimit
  keepFolders <- view redKeepFolders
  case input a of
    Just i -> do
      res <- fmap snd <$> L.withLogger (runCommand fp timelimit command i)
      when keepFolders (removePathForcibly fp)
      let judgment = case resultOutput res of
            Just m
              | checkExpectation expectation m ->
                Success
              | otherwise ->
                Failure
            Nothing ->
              Timeout
      return (judgment, Just res)
    Nothing -> do
      return (Bad, Nothing)

-- * The Problem Type


-- | A `ReductionProblem` is something that we can reduce, by running a program
-- on with the correct inputs.
data Problem s = Problem
  { initial     :: !s
  -- ^ An initial value to start reducing
  , input       :: s -> Maybe CmdInput
  -- ^ An way of computing a CmdInput from the problem
  , metric      :: AnyMetric s
  -- ^ An way of computing metrics of s
  , expectation :: !Expectation
  -- ^ The expected output of the CmdInput
  , command     :: !CmdTemplate
  -- ^ The command template to run.
  }

-- | Update the problem. This is useful if some partial reduction was achieved.
updateProblem :: (s -> s) -> Problem s -> Problem s
updateProblem fs p@Problem{..} = p { initial = fs initial }

-- | Set the problem to a new begining value
resetProblem :: s -> Problem s -> Problem s
resetProblem s = updateProblem (const s)

-- | Lift the problem to a new domain. Requires that the new domain is
-- isomorphic to the original domain.
liftProblem :: (s -> t) -> (t -> s) -> Problem s -> Problem t
liftProblem st ts =
  refineProblem ((ts,) . st)

-- | Given the original definition of the problem, refine the problem.
refineProblem :: (s -> (t -> s, t)) -> Problem s -> Problem t
refineProblem f Problem {..} =
  Problem t (input . tf) (tf `contramap` metric) expectation command
  where
    (tf, t) = f initial

setupProblemFromFile ::
  (MonadReductor env m, HasPredicateOptions env)
  => FilePath
  -> CmdTemplate
  -> FilePath
  -> m (Maybe (Problem CmdInput))
setupProblemFromFile workDir template inputf = do
  dirtree <- L.phase "Reading inputs" $ do
    liftIO $ readRelativeDirTree BL.readFile inputf

  setupProblem workDir template (inputFromDirTree (takeBaseName inputf) dirtree)

-- | Setup the problem from a command.
setupProblem ::
  (MonadReductor env m, HasPredicateOptions env)
  => FilePath
  -> CmdTemplate
  -> CmdInput
  -> m (Maybe (Problem CmdInput))
setupProblem workDir template a = L.phase "Calculating Initial Problem" $ do
  timelimit <- view redPredicateTimelimit
  opts <- view predicateOptions
  (snd . resultOutput <$> runCommand workDir timelimit template a) >>= \case
    Just output ->
      return . Just
      $ Problem a (Just . id) emptyMetric (zipExpectation opts output) template
    Nothing ->
      return Nothing

-- * Problem Refinements

-- | Get an indexed list of elements, this enables us to differentiate between stuff.
toIndexed :: Problem [a] -> Problem [Int]
toIndexed = toIndexed' . toFixedLenght

-- | Make the problem fixed length.
toFixedLenght :: Problem [a] -> Problem (V.Vector (Maybe a))
toFixedLenght = liftProblem (V.map Just . V.fromList) (catMaybes . V.toList)

-- | Turn a problem of reducing maybe values to one of reducting a list of integers.
toIndexed' :: Problem (V.Vector (Maybe a)) -> Problem [Int]
toIndexed' Problem {..} =
  Problem indicies (input . displ) (displ `contramap` metric) expectation command
  where
   nothings = V.map (const Nothing) initial
   indicies = V.toList . V.map fst . V.indexed $ initial
   displ ids = nothings V.// [(i, join $ initial V.!? i) | i <- ids]

-- | Create a stringed version that also measures the cl
toStringified :: (a -> Char) -> Problem [a] -> Problem [Int]
toStringified fx =
  toIndexed'
  . meassure (Stringify . V.toList . V.map (maybe '·' fx))
  . toFixedLenght

-- | Add a metric to the problem
meassure :: Metric r => (s -> r) -> Problem s -> Problem s
meassure sr p =
  p { metric = addMetric sr . metric $ p }

-- | Get an indexed list of elements, this enables us to differentiate between stuff.
toClosures :: Ord n => [Edge e n] -> Problem [n] -> Problem [IS.IntSet]
toClosures edges' = refineProblem refined
  where
    refined items = (fs, closures graph)
      where
        fs = map (nodeLabel . (nodes graph V.!)) . IS.toList . IS.unions
        (graph, _) = buildGraphFromNodesAndEdges (map (\a -> (a,a)) items) edges'

-- | Given a 'Reduction' from s given t, make the problem to reduce
-- a list of t's with indicies.
toReductionList :: Reduction s t -> Problem s -> Problem (V.Vector (Maybe t))
toReductionList red =
  refineProblem $ \s ->
    ( flip (limiting red) s . (\v i -> maybe False (const True) $ v V.!? i)
    , V.map Just . V.fromList $ toListOf (subelements red) $ s
    )

-- | Get an inde
toReductionTree' :: Reduction s s -> Problem s -> Problem (S.Set (NE.NonEmpty Int))
toReductionTree' red =
  refineProblem $ \s ->
    ( flip (limit $ treeReduction red) s . flip S.member
    , S.fromList $ indicesOf (treeReduction red) s
    )

toReductionTree :: Reduction s s -> Problem s -> Problem [[Int]]
toReductionTree red =
  liftProblem
  (fmap NE.toList . S.toAscList)
  (S.fromList . catMaybes . fmap NE.nonEmpty)
  . toReductionTree' red

-- * Expectation

-- | An `Expectation` can or cannot be meet by a `CmdOutputSummary`
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

-- | Zip the PredicateOptions with a CmdOutputSummary to build an Expectation
zipExpectation :: PredicateOptions -> CmdOutputSummary -> Expectation
zipExpectation PredicateOptions{..} CmdOutputSummary{..} =
  Expectation
  (guard predOptPreserveExitCode $> outputCode)
  (guard predOptPreserveStdout   $> outputOut)
  (guard predOptPreserveStderr   $> outputErr)

-- | Check if the output matches the expectation.
checkExpectation :: Expectation -> CmdOutputSummary -> Bool
checkExpectation Expectation{..} CmdOutputSummary{..} =
  maybe True (outputCode ==) expectedExitCode
  && maybe True (outputOut ==) expectedStdout
  && maybe True (outputErr ==) expectedStderr
