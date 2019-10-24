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
  , problemCommand
  , problemDescription
  , problemExpectation
  , problemExtractBase

  -- ** Constructors
  , setupProblem
  , setupProblemFromFile
  , PredicateOptions (..)
  , HasPredicateOptions (..)

  -- ** Run the problem
  , MonadReductor
  , runReductionProblem
  , checkSolution

  , ReductionOptions (..)
  , HasReductionOptions (..)
  , defaultReductionOptions

  -- ** Combinators
  , updateProblem
  , resetProblem
  , liftProblem
  , refineProblem
  , refineProblemA

  -- ** Problem Refinements
  , toReductionList
  , toReductionTree
  , toReductionDeep
  , toGraphReductionDeep
  , toGraphReductionDeepM

  , meassure

  , toIndexed
  , toFixedLenght
  , toIndexed'
  , toStringified
  , toClosures

  -- * Expectation

  , Expectation (..)
  , expectedStdout
  , expectedStderr
  , expectedExitCode
  , checkExpectation
  , zipExpectation

  -- * Utils
  , reductionGraphM
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
import qualified Data.ByteString            as BS

-- containers
import qualified Data.IntSet                as IS
import qualified Data.Set                   as S
import qualified Data.Map.Strict            as M

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


-- | A `ReductionProblem` is something that we can reduce, by running a program
-- with the correct inputs. It accepts two types `a` and `s`. `a` is the base
-- value of the problem. It is the one we can convert to and from. `s` is the
-- reduction value. The reduction value is the type that we try to reduce.
data Problem a s = Problem
  { _problemInitial     :: !s
  -- ^ An initial value to start reducing
  , _problemExtractBase :: ! (s -> Maybe a)
  -- ^ The definition of how to go from the Initial value to the base
  -- value.
  , _problemDescription :: ! (a -> CmdInput)
  -- ^ An way of computing a CmdInput from the problem
  , _problemMetric      :: ! (AnyMetric (Maybe s))
  -- ^ An way of computing metrics of s
  , _problemExpectation :: !Expectation
  -- ^ The expected output of the CmdInput
  , _problemCommand     :: !CmdTemplate
  -- ^ The command template to run.
  }

-- | An `Expectation` can or cannot be meet by a `CmdOutputSummary`
data Expectation = Expectation
  { _expectedExitCode :: Maybe ExitCode
  , _expectedStdout   :: Maybe Sha256
  , _expectedStderr   :: Maybe Sha256
  } deriving (Show, Eq)

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
  , _redTryInitial         :: !Bool
  -- ^ try if the initial problem is good (recored as 0000)
  } deriving (Show, Eq)

makeLenses ''Problem
makeLenses ''Expectation

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
  , _redTryInitial = False
  }

makeClassy ''ReductionOptions

-- | Predicate options, defines which elements to check
data PredicateOptions = PredicateOptions
  { _predOptPreserveExitCode :: !Bool
  , _predOptPreserveStdout   :: !Bool
  , _predOptPreserveStderr   :: !Bool
  , _predOptPriorExpectation :: !Expectation
  } deriving (Show, Eq)

makeClassy ''PredicateOptions

-- | A short hand for a monad reductor
type MonadReductor env m =
  (HasReductionOptions env, L.HasLogger env, MonadReader env m, MonadUnliftIO m)

-- | Given a filepath to the
setupProblemFromFile ::
  (MonadReductor env m, HasPredicateOptions env)
  => FilePath
  -- ^ The working directory
  -> CmdTemplate
  -- ^ The command template
  -> FilePath
  -- ^ The filepath to load the input from
  -> m (Maybe (Problem (DirTree BL.ByteString) (DirTree BL.ByteString)))
setupProblemFromFile workDir template inputf = do
  dirtree <- L.phase "Reading inputs" $ do
    liftIO $ readDirTree (fmap BL.fromStrict . BS.readFile) inputf

  setupProblem
    workDir template
    dirtree
    (inputFromDirTree (takeBaseName inputf) . asRelativeDirTree)

-- | Setup the problem from a command. Might fail if the command is not
-- satified.
setupProblem ::
  (MonadReductor env m, HasPredicateOptions env)
  => FilePath
  -- ^ The working directory
  -> CmdTemplate
  -- ^ the command template
  -> a
  -- ^ The input
  -> (a -> CmdInput)
  -- ^ a way of turning the input in to a command
  -> m (Maybe (Problem a a))
setupProblem workDir template a desc = L.phase "Calculating Initial Problem" $ do
  timelimit <- view redPredicateTimelimit
  opts <- view predicateOptions

  result <- runCommand workDir timelimit template (desc a)

  case snd . resultOutput $ result of
    Just output -> do
      unless (checkExpectation (_predOptPriorExpectation opts) output)
        $ L.warn "Expectations not met. Continuing with requested expectations"
      return . Just
        $ Problem
        { _problemInitial = a
        , _problemExtractBase = Just
        , _problemDescription = desc
        , _problemExpectation = (zipExpectation opts output <> _predOptPriorExpectation opts)
        , _problemCommand = template
        , _problemMetric = emptyMetric
        }
    Nothing ->
      return $ Nothing


-- | `runReduction` is the main function of this package. It is a wrapper
-- around a strategy to make it work with a problem.
runReductionProblem ::
  forall a s env m.
  MonadReductor env m
  => FilePath
  -- ^ The work directory
  -> Reducer IO s
  -- ^ The reducer to use on the problem
  -> Problem a s
  -- ^ The `Problem` to reduce
  -> m (Maybe ReductionException, s)
runReductionProblem wf reducer p = do
  opts <- view reductionOptions
  env <- ask
  doTryIntial <- view redTryInitial

  createDirectory wf
  withCurrentDirectory wf . liftIO $ do
    record <- setupMetric (p ^. problemMetric) (opts ^. redMetricsFile)
    iterRef <- newIORef 1
    succRef <- newIORef (p ^. problemInitial)


    when doTryIntial . flip runReaderT env $ do
      let
        fp = "0000"
        s = p ^. problemInitial

      L.info $ "Trying (Initial " <> L.displayString fp <> ")"
      L.debug $ " Metric: " <> displayAnyMetric (p ^. problemMetric) (Just s)

      (judgment, result) <- checkSolution fp p s

      L.info $ (L.displayString $ showJudgment judgment)

      liftIO $ record (MetricRow (Just s) 0 fp judgment result)

    start <- liftIO $ getCurrentTime

    ee <- goWith (p ^. problemInitial) $ \s -> flip runReaderT env $ do
      (fp, _diff) <- checkTimeouts start iterRef opts

      L.info $ "Trying (Iteration " <> L.displayString fp <> ")"
      L.debug $ " Metric: " <> displayAnyMetric (p ^. problemMetric) (Just s)

      (judgment, result) <- checkSolution fp p s

      L.info $ (L.displayString $ showJudgment judgment)

      liftIO $ record (MetricRow (Just s) _diff fp judgment result)

      let success = judgment == Success

      when success $ writeIORef succRef s
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


-- | Given a folder to run in, check that a problem has been solved with `a`.
-- this function should be run in some workfolder
checkSolution ::
  MonadReductor env m
  => FilePath
  -> Problem a s
  -> s
  -> m (Judgment, Maybe (CmdResult (Maybe CmdOutputSummary)))
checkSolution fp Problem{..} s = do
  timelimit <- view redPredicateTimelimit
  keepFolders <- view redKeepFolders
  case _problemExtractBase s of
    Just a -> do
      let runCmd = runCommand fp timelimit _problemCommand (_problemDescription a)
      res <- finally
        ( fmap snd <$> L.withLogger runCmd )
        ( unless keepFolders (removePathForcibly fp) )
      let judgment = case resultOutput res of
            Just m
              | checkExpectation _problemExpectation m ->
                Success
              | otherwise ->
                Failure
            Nothing ->
              Timeout
      return (judgment, Just res)
    Nothing -> do
      return (Bad, Nothing)

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

-- * The Problem Type


-- | Update the problem. This is useful if some partial reduction was achieved.
updateProblem :: (s -> s) -> Problem a s -> Problem a s
updateProblem fs = problemInitial %~ fs

-- | Set the problem to a new begining value
resetProblem :: s -> Problem a s -> Problem a s
resetProblem s = updateProblem (const s)

-- | Lift the problem to a new domain. Requires that the new domain is
-- isomorphic to the original domain.
liftProblem :: (s -> t) -> (t -> s) -> Problem a s -> Problem a t
liftProblem st ts =
  refineProblem ((Just . ts,) . st)

-- | Given the original definition of the problem, refine the problem.
refineProblem :: (s -> (t -> Maybe s, t)) -> Problem a s -> Problem a t
refineProblem f p@Problem {..} =
  p { _problemInitial = t
    , _problemExtractBase = (tf >=> _problemExtractBase)
    , _problemMetric = contramap (>>= tf) _problemMetric
    }
  where
    (tf, t) = f _problemInitial

-- | It is also possible to refine the problem and access a monad or applicative.
-- This is usefull in some situations were external controll is needed for refinement.
refineProblemA :: Functor m
  => (s -> m (t -> Maybe s, t))
  -> Problem a s
  -> m (Problem a t)
refineProblemA f p@Problem {..} = do
  f _problemInitial <&> \(tf, t) ->
    p { _problemInitial = t
      , _problemExtractBase = (tf >=> _problemExtractBase)
      , _problemMetric = contramap (>>= tf) _problemMetric
      }

refineProblemA' :: Functor m
  => (s -> m (x, (t -> Maybe s, t)))
  -> Problem a s
  -> m (x, Problem a t)
refineProblemA' f p@Problem {..} = do
  f _problemInitial <&> \(x, (tf, t)) ->
    ( x
    , p { _problemInitial = t
      , _problemExtractBase = (tf >=> _problemExtractBase)
      , _problemMetric = contramap (>>= tf) _problemMetric
      }
    )


-- * Problem Refinements

-- | Get an indexed list of elements, this enables us to differentiate between stuff.
toIndexed :: Problem a [s] -> Problem a [Int]
toIndexed = toIndexed' . toFixedLenght

-- | Make the problem fixed length.
toFixedLenght :: Problem a [s] -> Problem a (V.Vector (Maybe s))
toFixedLenght = liftProblem (V.map Just . V.fromList) (catMaybes . V.toList)

-- | Turn a problem of reducing maybe values to one of reducting a list of integers.
toIndexed' :: Problem a (V.Vector (Maybe s)) -> Problem a [Int]
toIndexed' = refineProblem fn where
  fn s = (Just . displ, indicies)
    where
      nothings = V.map (const Nothing) s
      indicies = V.toList . V.map fst . V.indexed $ s
      displ ids = nothings V.// [(i, join $ s V.!? i) | i <- ids]

-- | Create a stringed version that also measures the cl
toStringified :: (s -> Char) -> Problem a [s] -> Problem a [Int]
toStringified fx =
  toIndexed'
  . meassure (Stringify . maybe "" (V.toList . V.map (maybe '·' fx)))
  . toFixedLenght

-- | Add a metric to the problem
meassure :: Metric r => (Maybe s -> r) -> Problem a s -> Problem a s
meassure sr = problemMetric %~ addMetric sr


-- | Given a 'Reduction' from s given t, make the problem to reduce
-- a list of t's with indicies.
toReductionList :: Reduction s t -> Problem a s -> Problem a (V.Vector (Maybe t))
toReductionList red =
  refineProblem $ \s ->
    ( Just . flip (limiting red) s . (\v i -> maybe False (const True) $ v V.!? i)
    , V.map Just . V.fromList $ toListOf (subelements red) $ s
    )

-- | Get an inde
toReductionTree' :: Reduction s s -> Problem a s -> Problem a (S.Set (NE.NonEmpty Int))
toReductionTree' red =
  refineProblem $ \s ->
    ( Just . flip (limit $ treeReduction red) s . flip S.member
    , S.fromList $ indicesOf (treeReduction red) s
    )

toReductionTree :: Reduction s s -> Problem a s -> Problem a [[Int]]
toReductionTree red =
  liftProblem
  (fmap NE.toList . S.toAscList)
  (S.fromList . catMaybes . fmap NE.nonEmpty)
  . toReductionTree' red

toReductionDeep :: PartialReduction s s -> Problem a s -> Problem a [[Int]]
toReductionDeep red =
  liftProblem (S.toAscList) (S.fromList) . toReductionDeep' red

toReductionDeep' :: PartialReduction s s -> Problem a s -> Problem a (S.Set [Int])
toReductionDeep' red =
  refineProblem $ \s ->
    ( flip (limit $ deepReduction red) s . flip S.member
    , S.fromList $ indicesOf (deepReduction red) s
    )

-- | Like reduction deep, but also calculates the graph to reduce with.
-- It automatically adds edges to parrent items
toGraphReductionDeep ::
  (Ord k) =>
  (s -> (k, [k]))
  -- ^ A key function
  -> PartialReduction s s
  -- ^ A partial reduction
  -> Problem a s
  -> Problem a [IS.IntSet]
toGraphReductionDeep keyfn red =
  snd . runIdentity
  . toGraphReductionDeepM
  (\s -> do
      let (mk, ks) = keyfn s
      return (mk, False, ks)
  ) red

-- | Like reduction deep, but also calculates the graph to reduce with.
-- It automatically adds edges to parrent items
toGraphReductionDeepM ::
  (Monad m, Ord k) =>
  (s -> m (k, Bool, [k]))
  -- ^ A key function, If the bool is true the item is required, and
  -- a closure should be calculated from it.
  -> PartialReduction s s
  -- ^ A partial reduction
  -> Problem a s
  -> m ( ( Graph () ([Int], k)
         , IS.IntSet
         , [IS.IntSet]
         )
       , Problem a [IS.IntSet]
       )
toGraphReductionDeepM keyfn red = refineProblemA' refined where
  refined s = do
    (graph, _) <- reductionGraphM keyfn red s
    let
      igraph = fmap (fst . fst) graph

      fromClosures cls = limit (deepReduction red) (`S.member` m) s where
        m = S.fromList . map (nodeLabel . (nodes igraph V.!))
          . IS.toList . IS.unions
          $ core:cls

      core =
        closure graph required

      required =
          map fst
          . filter (\(_, (_, b)) -> b)
          . itoList
          $ nodeLabels graph

      _closures =
        closures graph

      _targets =
        filter (not . IS.null)
        . map (IS.\\ core)
        $ _closures

    pure
      ( (fmap fst graph, core, _targets)
      , ( fromClosures, _targets)
      )

-- Calculate a reduction graph from a key function.
reductionGraphM ::
  (Monad m, Ord k)
  => (s -> m (k, Bool, [k]))
  -- ^ A key function
  -> PartialReduction s s
  -- ^ A partial reduction
  -> s
  -> m ( Graph () (([Int], k), Bool)
       , [Int] -> Maybe Vertex
       )
reductionGraphM keyfn red s = do
  nodes_ <- forM (itoListOf (deepSubelements red) s) $ \(i, a) -> do
    (mk, b, ks) <- keyfn a
    return (i, b, mk, ks)

  let keymap = M.fromListWith S.union
        [ (k, S.singleton n) | (n, _, k, _) <- nodes_ ]

  nodes <- forM nodes_ $ \(n, b, k, ks) -> do
    let _edges = addInit n . flip concatMap ks $ \k' ->
          fmap (,())
          . S.toList
          . fromMaybe S.empty
          $ keymap M.!? k'
   
    return (((n, k), b), n, _edges)

  return $ buildGraph nodes
  where
    addInit = (\case [] -> id; a -> ((tail a, ()):))

-- | Get an indexed list of elements, this enables us to differentiate between stuff.
toClosures :: Ord n => [Edge e n] -> Problem a [n] -> Problem a [IS.IntSet]
toClosures edges' = refineProblem refined where
  refined items = (Just . fs, closures graph)
    where
      fs = map (nodeLabel . (nodes graph V.!)) . IS.toList . IS.unions
      (graph, _) = buildGraphFromNodesAndEdges (map (\a -> (a,a)) items) edges'
-- * Expectation


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
  (guard _predOptPreserveExitCode $> outputCode)
  (guard _predOptPreserveStdout   $> outputOut)
  (guard _predOptPreserveStderr   $> outputErr)

-- | Check if the output matches the expectation.
checkExpectation :: Expectation -> CmdOutputSummary -> Bool
checkExpectation Expectation{..} CmdOutputSummary{..} = and
  [ maybe True (outputCode ==) _expectedExitCode
  , maybe True (outputOut ==) _expectedStdout
  , maybe True (outputErr ==) _expectedStderr
  ]
