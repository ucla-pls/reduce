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
import Control.Lens.Iso
import Control.Lens

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

-- bytestring
import qualified Data.ByteString.Char8                 as BS
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.ByteString.Lazy.Char8            as BLC

-- mtl
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe

-- base
import qualified Data.List                             as L
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

data PredicateOptions = PredicateOptions
  { predOptExpectedStatus :: !ExitCode
  , predOptPreserveStdout :: !Bool
  , predOptPreserveStderr :: !Bool
  , predOptWorkFolder     :: !FilePath
  , predOptKeepFolders    :: !Bool
  , predOptCmd            :: !Cmd
  } deriving (Show, Eq)

-- | Creates a predicate from the CheckOptions and CmdOptions.
toPredicateM ::
 (L.HasLogger env, MonadReader env m, MonadUnliftIO m)
 => PredicateOptions
 -> (a -> m CmdInput)
 -> a
 -> m (Maybe (PredicateM m a))
toPredicateM PredicateOptions {..} setup a = do
  L.phase "Initial run" $ do
    runCmd setup (predOptWorkFolder </> "initial") predOptCmd a >>= \case
      Just (ec, oh, eh)
        | ec /= predOptExpectedStatus ->
          return $ Nothing
        | otherwise -> do
            let
              pred =
                PredicateM $
                ( (\(fp, a) ->
                     runCmd setup (predOptWorkFolder </> fp) predOptCmd a)
                >=> testM oh eh
                )
            Just <$> wrapPredicate pred
      Nothing ->
        return $ Nothing
  where
    testM oh eh x = do
      let p = testp oh eh x
      L.info $ if p then "success" else "failure"
      return p
    testp oh eh = \case
      Just (ec', oh', eh') ->
        ec' == predOptExpectedStatus
        && (not predOptPreserveStdout || oh' == oh)
        && (not predOptPreserveStderr || eh' == eh)
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
  (Monad m)
  => ReducerName
  -> PredicateM m IS.IntSet
  -> [IS.IntSet]
  -> m (Maybe [IS.IntSet])
setreduce reducer p ls = do
  case reducer of
    Ddmin ->
      toSetReducer unsafeDdmin p ls
    Linear ->
      toSetReducer linearReduction p ls
    Binary ->
      setBinaryReduction p ls
