{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ViewPatterns        #-}
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

-}
module Control.Reduce.Util
  (
    runCmd

  , CheckOptions (..)
  , toPredicateM

  , ReducerOptions (..)
  , ReducerName (..)
  , reduce

  , exitCodeFromInt
  , exitCodeToInt

  ) where

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

-- bytestring
import qualified Data.ByteString.Char8                 as BS
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.ByteString.Lazy.Char8            as BLC

-- mtl
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe

-- base
import           System.Exit
import           Text.Printf

-- directory-tree
import           System.Directory.Tree

-- process
import           System.Process                        (showCommandForUser)

-- reduce-util
import           Control.Reduce.Util.Logger            as L
import           Control.Reduce.Util.CliPredicate
import           System.Process.Consume

-- reduce
import           Control.Reduce
import           Data.Functor.Contravariant.PredicateM

-- checkConfig :: ! CheckConfig

data CheckOptions = CheckOptions
  { expectedStatus :: ExitCode
  , preserveStdout :: Bool
  , preserveStderr :: Bool
  } deriving (Show, Eq)

-- | Creates a predicate from the CheckOptions and CmdOptions.
toPredicateM ::
 (HasLogger env, MonadReader env m, MonadUnliftIO m)
 => CheckOptions
 -> (a -> m CmdInput)
 -> FilePath
 -> Cmd
 -> a
 -> m (Maybe (PredicateM m (FilePath, a)))
toPredicateM CheckOptions {..} setup workFolder cmd a = do
  phase "Initial run" $ do
    runCmd setup (workFolder </> "initial") cmd a >>= \case
      Just (ec, oh, eh)
        | ec /= expectedStatus ->
          return $ Nothing
        | otherwise ->
          return . Just .
            PredicateM $
            ( (\(fp, a) -> runCmd setup (workFolder </> fp) cmd a)
             >=> testM oh eh
            )
      Nothing ->
        return $ Nothing
  where
    testM oh eh x = do
      let p = testp oh eh x
      L.info $ if p then "success" else "failure"
      return p

    testp oh eh = \case
      Just (ec', oh', eh') ->
        ec' == expectedStatus
        && (not preserveStdout || oh' == oh)
        && (not preserveStderr || eh' == eh)
      Nothing ->
        False

-- | Wrap a predicate and have it run in the folders
wrapPredicate ::
  (HasLogger env, MonadReader env m, MonadUnliftIO m)
  => String
  -> PredicateM m (FilePath, a)
  -> m (PredicateM m a)
wrapPredicate name pred = do
  ref <- newIORef (0 :: Int)
  return . PredicateM $ go ref
  where
    go ref a = do
      x <- liftIO $ atomicModifyIORef ref (\x -> (succ x, x))
      let phaseName =
            "Iteration ("
              <> Builder.fromString name
              <> ")"
              <-> displayf "%04d" x
      phase phaseName $
        runPredicateM pred (printf "%s/%04d" name x, a)

data ReducerOptions = ReducerOptions
  { reducer        :: ReducerName
  , workFolder     :: FilePath
  , keepIterations :: Bool
  } deriving (Show)

data ReducerName
  = Ddmin
  | Linear
  | Binary
  deriving (Show)

-- | Reduce using the reducer options.
reduce ::
  (HasLogger env, MonadReader env m, MonadUnliftIO m)
  => ReducerOptions
  -> String
  -> PredicateM m [a]
  -> [a]
  -> m (Maybe [a])
reduce ReducerOptions {..} name p ls = do
  phase ("Reduction" <-> Builder.fromString name) $ do
    case reducer of
      Ddmin ->
        unsafeDdmin p ls
      Linear ->
        runMaybeT (unsafeLinearReduction (asMaybeGuard p) ls)
      Binary ->
        binaryReduction p ls
