{-# LANGUAGE DeriveGeneric       #-}
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
  ( PredicateOptions (..)
  , toPredicateM

  , ReducerName (..)
  , reduce
  , setreduce

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

-- cassava
import qualified Data.Csv as C

-- bytestring
import qualified Data.ByteString.Char8                 as BS
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.ByteString.Lazy.Char8            as BLC

-- mtl
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe

-- base
import qualified Data.List                             as L
import           GHC.Generics                          (Generic)
import           System.Exit
import           Text.Printf
import           Data.Maybe

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


data PredicateOptions = PredicateOptions
  { predOptExpectedStatus :: !ExitCode
  , predOptPreserveStdout :: !Bool
  , predOptPreserveStderr :: !Bool
  , predOptMetrics        :: !FilePath
  , predOptWorkFolder     :: !FilePath
  , predOptKeepFolders    :: !Bool
  , predOptCmd            :: !Cmd
  } deriving (Show, Eq)

data Metric a = Metric
  { metricContent :: !a
  , metricFolder  :: !String
  , metricResults :: !(Maybe CmdResult)
  , metricSuccess :: !Bool
  } deriving (Show, Eq, Generic)


instance C.DefaultOrdered a => C.DefaultOrdered (Metric a) where
  headerOrder _ =
    C.headerOrder (undefined :: a)
    <> C.header
    [ "folder"
    , "success"
    , "setup time"
    , "run time"
    , "status"
    , "stdout (length)"
    , "stdout (sha256)"
    , "stderr (length)"
    , "stderr (sha256)"
    ]

instance C.ToNamedRecord a => C.ToNamedRecord (Metric a) where
  toNamedRecord Metric {..} =
    C.toNamedRecord metricContent <>
    C.namedRecord
    ["folder" C..= (metricFolder :: String)
    , "setup time" C..= maybe (-1) resultSetupTime metricResults
    , "run time" C..= maybe (-1) resultRunTime metricResults
    , "status" C..= maybe (-1) (exitCodeToInt . resultExitCode) metricResults
    , "stdout (length)"
      C..= maybe (-1 :: Int) (fromIntegral . snd . resultStdout) metricResults
    , "stdout (sha256)"
      C..= maybe "-" (showHash . fst . resultStdout) metricResults
    , "stderr (length)"
      C..= maybe (-1 :: Int) (fromIntegral . snd . resultStderr) metricResults
    , "stderr (sha256)"
      C..= maybe "-" (showHash . fst . resultStderr) metricResults
    , "success" C..= (if metricSuccess then "true" else "false" :: String)
    ]

-- | Creates a predicate from the CheckOptions and CmdOptions.
toPredicateM ::
 (L.HasLogger env, C.ToNamedRecord c, C.DefaultOrdered c, MonadReader env m,
 MonadUnliftIO m)
 => PredicateOptions
 -> (a -> m CmdInput)
 -> (a -> c)
 -> a
 -> m (Maybe (PredicateM m a))
toPredicateM PredicateOptions {..} setup cf a = do
  L.phase "Initial run" $ do
    let folder = predOptWorkFolder </> "initial"
    cmdres <- runCmd setup folder predOptCmd a
    pred' <- case cmdres of
      Just CmdResult {..}
        | resultExitCode /= predOptExpectedStatus ->
          return $ Nothing
        | otherwise -> do
            Just <$> wrapPredicate
              ( PredicateM $ predicate resultStdout resultStderr )
      Nothing ->
        return $ Nothing
    liftIO .
      BLC.writeFile predOptMetrics
      $ C.encodeDefaultOrderedByName
      [ Metric (cf a) folder cmdres (isJust pred')]
    return pred'
  where
    predicate oh eh (fp, a) = do
      let folder = (predOptWorkFolder </> fp)
      res <- runCmd setup folder predOptCmd a
      let succ = p res
      L.info $ if succ then "success" else "failure"
      liftIO .
        BLC.appendFile predOptMetrics
        $ C.encodeDefaultOrderedByNameWith
          (C.defaultEncodeOptions { C.encIncludeHeader = False })
          [ Metric (cf a) folder res succ ]
      return succ
      where
        p = \case
          Just CmdResult {..} ->
            resultExitCode == predOptExpectedStatus
            && (not predOptPreserveStdout || resultStdout == oh)
            && (not predOptPreserveStderr || resultStderr == eh)
          Nothing ->
            False

    wrapPredicate ::
      (L.HasLogger env, MonadReader env m, MonadUnliftIO m)
      => PredicateM m (FilePath, a)
      -> m (PredicateM m a)
    wrapPredicate pred = do
      ref <- newIORef (0 :: Int)
      return . PredicateM $ go ref
      where
        go ref a = do
          x <- liftIO $ atomicModifyIORef ref (\x -> (succ x, x))
          let phaseName =
                "Iteration " L.<-> L.displayf "%04d" x
          L.phase phaseName $
            runPredicateM pred (printf "%04d" x, a)


-- | The name of the reducer
data ReducerName
  = Ddmin
  | Linear
  | Binary
  deriving (Show)

-- | Reduce using the reducer options.
reduce ::
  (Monad m)
  => ReducerName
  -> Maybe ([b] -> Int)
  -> PredicateM m a
  -> AnIso' a [b]
  -> a
  -> m (Maybe a)
reduce reducer costf p f a = do
  let p' = view (from f) `contramap` p
  (fmap . fmap $ view (from f)) $
    case reducer of
      Ddmin ->
        unsafeDdmin p' input
      Linear ->
        runMaybeT
          (unsafeLinearReduction (asMaybeGuard p') input)
      Binary ->
        case costf of
          Nothing -> binaryReduction p' input
          Just cf ->
            genericBinaryReduction cf p' (a ^. cloneIso f)
  where
    input =
      maybe id (\cf -> L.sortOn (cf . (:[]))) costf $ a ^. cloneIso f

setreduce ::
  forall m a.
  (Monad m)
  => ReducerName
  -> PredicateM m a
  -> (a -> [IS.IntSet], IS.IntSet -> a)
  -> a
  -> m (Maybe a)
setreduce reducer p (fto, ffrom) a = do
  let p' = ffrom `contramap` p
  (fmap . fmap $ ffrom . IS.unions) $
    case reducer of
      Ddmin ->
        toSetReducer unsafeDdmin p' input
      Linear ->
        toSetReducer linearReduction p' input
      Binary ->
        setBinaryReduction p' input
  where
    input = fto a
