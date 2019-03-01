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
  -- , reduce
  -- , setreduce

  , module Control.Reduce.Util.CliPredicate
  ) where

-- lens
import           Control.Lens
import           Control.Lens.Iso

-- typed-process
import           System.Process.Typed

-- filepath
import           System.FilePath

-- text
import qualified Data.Text.Lazy.Builder                as Builder
import qualified Data.Text.Lazy.Encoding               as Text

-- unliftio
import           UnliftIO
import           UnliftIO.Directory

-- containers
import qualified Data.IntSet                           as IS

-- vector
import qualified Data.Vector                           as V

-- cassava
import qualified Data.Csv                              as C

-- time
import           Data.Time

-- bytestring
import qualified Data.ByteString.Char8                 as BS
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.ByteString.Lazy.Char8            as BLC

-- mtl
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe

-- base
import qualified Data.List                             as L
import           Data.Maybe
import           GHC.Generics                          (Generic)
import           System.Exit
import           Text.Printf

-- directory-tree
import           System.Directory.Tree

-- process
import           System.Process                        (showCommandForUser)

-- contravariant
import           Data.Functor.Contravariant

-- reduce-util
import           Control.Reduce.Util.CliPredicate
import qualified Control.Reduce.Util.Logger            as L
import           System.Process.Consume

-- reduce
import           Control.Reduce
import           Data.Functor.Contravariant.PredicateM

-- -- cassava
-- import qualified Data.Csv

data Predicate a = Predicate
  { expected :: !CmdOutput
  , command :: !Command a (Maybe CmdOutput)
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

data PredicateOptions = PredicateOptions
  { predOptPreserveExitCode :: !Bool
  , predOptPreserveStdout :: !Bool
  , predOptPreserveStderr :: !Bool
  } deriving (Show, Eq)

makeCommandPredicate ::
  PredicateOptions
  -> CmdOutput
  -> Command a (Maybe CmdOutput)
  -> Command a Bool
makeCommandPredicate opt output =
  postprocess (checkOutputPreserved opt output)

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

-- | The name of the reducer
data ReducerName
  = Ddmin
  | Linear
  | Binary
  deriving (Show, Eq)

data ReductionOptions = ReductionOptions
  { redOptPredicateOptions :: !PredicateOptions
  , redOptTotalTimeout     :: !Double
  , redOptMaxIterations    :: !Int
  , redOptMetrics          :: !FilePath
  , redOptWorkFolder       :: !FilePath
  , redOptKeepFolders      :: !Bool
  , redOptName             :: !ReducerName
  } deriving (Show, Eq)

data PredicateException
  = PredicateTimedOut
  | PredicateIterationsExceeded
  deriving (Show)

instance Exception PredicateException


reduceList ::
  (Metric b)
  => ReductionOptions
  -> ([a] -> b)
  -> Predicate [a]
  -> [a]
  -> L.Logger [a]
reduceList opts metric cmd a = do
  setupReduction


setupReduction ::
  (Metric b)
  => ReductionOptions
  -> CmdOutput
  -> Predicate a CmdOutput
  -> IO (PredicateM L.Logger (FilePath, (b, a)))
setupReduction ReductionOptions {..} output cmd = do
  start <- getCurrentTime
  return . PredicateM $ \(fp, (b, a)) -> do
    now <- liftIO $ getCurrentTime
    L.info $ "Trying: " <> displayMetric b

    let diff = now `diffUTCTime` start

    when (0 < redOptTotalTimeout && redOptTotalTimeout < realToFrac diff)
      . liftIO . throwIO $ PredicateTimedOut

    res <- runCommand (redOptWorkFolder </> fp) cmd a

    let succ = checkOutputPreserved redOptPredicateOptions output (resultOutput res)
    L.info $ if succ then "success" else "failure"

    liftIO
      . BLC.appendFile redOptMetrics
      $ C.encodeDefaultOrderedByNameWith
        ( C.defaultEncodeOptions { C.encIncludeHeader = False } )
        [ MetricRow b diff fp res succ ]

    return succ


-- getExpectedOutput :: PredicateOptions -> (a -> m CmdInput) -> FilePath -> a -> m (CmdResult (Maybe CmdOutput))
-- getExpectedOutput PredicateOptions {..} setup folder a = do
--     cmdres <- runCmd predOptCmd (setup a) folder
--     case cmdres of
--       Just (CmdResult ts tr (output@CmdOutput {..}))
--         | outputCode /= predOptExpectedStatus ->
--           return $ Nothing
--         | otherwise -> return $ Just cmdres
--       Nothing ->
--         return $ Nothing


-- mkReductionProblem ::
--   forall env b m a.
--   (L.HasLogger env, Metric b, MonadReader env m, MonadUnliftIO m)
--   => PredicateOptions
--   -> (a -> m CmdInput)
--   -> a
--   -> m (Maybe (ReductionProblem m (b, a) a))
-- mkReductionProblem popt@(PredicateOptions {..}) setup a = do
--   L.phase "Initial run" $ do
--     let folder = predOptWorkFolder </> "initial"

--     cmdres <- getExpectedOutput popt setup folder a

--     liftIO .
--       BLC.writeFile predOptMetrics
--       $ C.encodeDefaultOrderedByName
--       [ MetricRow (initialMetric :: b) 0 "initial" cmdres (isJust pred)]

--     return $ ReductionProblem Nothing (initialMetric, a) snd <$> pred

--   where
--     wrapPredicate ::
--       (L.HasLogger env, MonadReader env m, MonadUnliftIO m)
--       => PredicateM m (FilePath, (b, a))
--       -> m (PredicateM m (b, a))
--     wrapPredicate pred = do
--       ref <- newIORef (0 :: Int)
--       return . PredicateM $ go ref
--       where
--         go ref a = do
--           x <- liftIO $ atomicModifyIORef ref (\x -> (succ x, x))
--           when (0 < predOptMaxIterations && predOptMaxIterations < x)
--             . liftIO . throwIO $ PredicateIterationsExceeded
--           let phaseName =
--                 "Iteration" L.<-> L.displayf "%04d" x
--           L.phase phaseName $
--             runPredicateM pred (printf "%04d" x, a)

-- {-# inlineable mkReductionProblem #-}


-- data ReductionProblem m a b = ReductionProblem
--   { reductionCost       :: Maybe (a -> Int)
--   , reductionModel      :: ReductionModel () a a
--   , reductionPredictate :: PredicateM m a
--   }

-- data ReductionModel b x a = ReductionModel
--   { redModel    :: b
--   , fromModelTo :: b -> (x, a)
--   }

-- model :: Metric x => b -> (b -> a) -> ((b, a) -> x) ->  ReductionModel x a b
-- model m fm tx = ReductionModel m $ \b -> let a = fm b in (tx (b, a), a)

-- applyModel :: ReductionModel x a c -> ReductionProblem m (x, a) b -> ReductionProblem m c b
-- applyModel (ReductionModel m fromModel) =
--   specializeReduction m fromModel

-- specializeReduction :: c -> (c -> a) -> ReductionProblem m a b -> ReductionProblem m c b
-- specializeReduction c ca (ReductionProblem {..}) =
--   ReductionProblem
--     (fmap (. ca) reductionCost)
--     c
--     (reductionRetrive . ca)
--     (ca `contramap` reductionPredictate)


-- -- | Reduce using the reducer options.
-- reduce ::
--   (Monad m)
--   => ReducerName
--   -> ReductionProblem m [a] b
--   -> m (Maybe b)
-- reduce reducer (ReductionProblem costf input f p) = do
--   fmap f <$> case reducer of
--     Ddmin ->
--       unsafeDdmin p sortedInput
--     Linear ->
--       runMaybeT
--         (unsafeLinearReduction (asMaybeGuard p) sortedInput)
--     Binary ->
--       case costf of
--         Nothing -> binaryReduction p sortedInput
--         Just cf ->
--           genericBinaryReduction cf p input
--   where
--     sortedInput =
--       maybe id (\cf -> L.sortOn (cf . (:[]))) costf $ input
-- {-# inline reduce #-}

-- setreduce ::
--   forall m a.
--   (Monad m)
--   => ReducerName
--   -> PredicateM m a
--   -> (IS.IntSet -> a)
--   -> [IS.IntSet]
--   -> m (Maybe a)
-- setreduce reducer p ffrom input = do
--   let p' = ffrom `contramap` p
--   (fmap . fmap $ ffrom . IS.unions) $
--     case reducer of
--       Ddmin ->
--         toSetReducer unsafeDdmin p' input
--       Linear ->
--         toSetReducer linearReduction p' input
--       Binary ->
--         setBinaryReduction p' input


-- * Metrics

class Metric a where
   order :: Const [BS.ByteString] a
   fields :: a -> [(BS.ByteString, BS.ByteString)]
   displayMetric :: a -> Builder.Builder
   initialMetric :: a

data MetricRow a = MetricRow
  { metricRowContent :: !a
  , metricRowTime    :: !NominalDiffTime
  , metricRowFolder  :: !String
  , metricRowResults :: !(CmdResult (Maybe CmdOutput))
  , metricRowSuccess :: !Bool
  } deriving (Show, Eq, Generic)

instance Metric a => C.DefaultOrdered (MetricRow a) where
  headerOrder _ =
    C.header $
    getConst (order :: Const [BS.ByteString] a)
    <> [ "folder"
       , "time"
       , "success"
       , "setup time"
       , "run time"
       , "status"
       , "stdout (length)"
       , "stdout (sha256)"
       , "stderr (length)"
       , "stderr (sha256)"
       ]

instance Metric a => C.ToNamedRecord (MetricRow a) where
  toNamedRecord MetricRow {..} =
    C.namedRecord $
    (fields metricRowContent) <>
    [ "folder" C..= metricRowFolder
    , "time" C..= (realToFrac metricRowTime :: Double)
    , "setup time" C..= resultSetupTime metricRowResults
    , "run time" C..= resultRunTime metricRowResults
    , "status" C..= maybe (-1) (exitCodeToInt . outputCode) result
    , "stdout (length)"
      C..= maybe (-1 :: Int) (fromIntegral . snd . outputOut) result
    , "stdout (sha256)"
      C..= maybe "-" (showHash . fst . outputOut) result
    , "stderr (length)"
      C..= maybe (-1 :: Int) (fromIntegral . snd . outputErr) result
    , "stderr (sha256)"
      C..= maybe "-" (showHash . fst . outputErr) result
    , "success" C..= (if metricRowSuccess then "true" else "false" :: String)
    ]
    where
      result = resultOutput $ metricRowResults

-- counted :: ([b] -> a) -> [b] -> ReductionModel Count a [b]
-- counted ba b =
--   model b ba (Count . length . fst)

-- data Count = Count { getCount :: Int }
--   deriving (Show)

-- instance Metric Count where
--   order = Const ["count"]
--   fields (Count a) =
--     ["count" C..= a ]
--   displayMetric (Count a) =
--     L.displayf "#%i elements" a
--   initialMetric = Count (-1)


-- displayed :: a -> (b -> Char) -> [b] -> ([b] -> a) -> ReductionModel Display a [Int]
-- displayed a toChar b ba =
--   model
--     idxs
--     (ba . catMaybes . map (v V.!?) . L.sort)
--     (displ . fst)

--   where
--     v = V.fromList b
--     str = V.map toChar v
--     idxs = V.toList . V.map fst . V.indexed $ v
--     displ a =
--       let is = IS.fromList a in
--       Display . V.toList $ V.imap (\i c -> if IS.member i is then c else '·') str

-- newtype Display = Display String

-- instance Metric Display where
--   order = Const ["display"]
--   fields (Display a) =
--     ["display" C..= a ]
--   displayMetric (Display a) =
--     Builder.fromString a

--   initialMetric = Display ""

-- displayFromString :: [b] -> Display b [Int]
-- displayFromString chs =
--   let
--     v = V.fromList chs
--     idxs = V.toList . V.map fst . V.indexed $ v
--   in
--   Display v idxs

-- getIntList :: Display b [Int] -> [Int]
-- getIntList (Display _ a) = a

-- compactString :: Display b [Int] -> [b]
-- compactString (Display v a) =
--   catMaybes . map (v V.!?) . L.sort $ a


-- data Listed a = Listed { getList :: [Bool], items :: a }
--   deriving (Functor, Show)

-- instance Metric (Listed [Char]) where
--   order = Const ["count", "listed"]
--   fields (Listed bs items) =
--     ["count" C..= (length $ filter id bs)
--     ,"listed" C..= zipWith (\b x -> if b then x else '·') bs items
--     ]
