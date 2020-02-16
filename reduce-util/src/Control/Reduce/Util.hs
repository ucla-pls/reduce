{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-|
Module      : Control.Reduce.Util
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

This module provides utilities, so that it is easier to write command-line
reducers.

-}
module Control.Reduce.Util
  ( reduction
  , reductionWith
  , ReducerName (..)

  -- * Re-Exports

  , module Control.Reduce.Problem

  -- * Utils
  , findOutputFile
  , versionInputFile

  , 
  ) where

-- unliftio
import           UnliftIO
import           UnliftIO.Directory

-- filepath
import           System.FilePath

-- mtl
import           Control.Monad.Reader

-- reduce
import           Control.Reduce

-- reduce-util
import           Control.Reduce.Problem
import qualified Control.Reduce.Util.Logger as L

-- | Like `reducitonWith` but in case you use the `Binary` reduction
-- type the cost function is just the length of the list.
reduction :: Monad m => ReducerName -> Reducer m [a]
reduction = reductionWith length

-- | Reduction given a cost function and the type of reduction.
reductionWith :: Monad m => ([a] -> Int) -> ReducerName -> Reducer m [a]
reductionWith cost = \case
  Ddmin  -> ddmin
  Linear -> linearReduction
  Binary -> genericBinaryReduction cost

-- | The name of the reducer
data ReducerName
  = Ddmin
  | Linear
  | Binary
  deriving (Show, Eq)

-- | Given an input filename and maybe an output name, move
-- either return the output or move the input name to a new version
-- and return the input name.
findOutputFile ::
     (MonadIO m, L.HasLogger c, MonadReader c m)
  => FilePath
  -> Maybe FilePath
  -> m FilePath
findOutputFile input =
  maybe (input <$ versionInputFile input) return

-- | Given an input filename, add the extension 'orig' to the input file
-- or if that file already exist create versions 'v1', 'v2', and so on.
versionInputFile ::
     (MonadIO m, L.HasLogger c, MonadReader c m)
  => FilePath
  -> m ()
versionInputFile input = do
  newpath <- liftIO $ firstAvailableEnding (1 :: Int) "orig"
  L.info $ "Moving input file to " <> L.display newpath
  liftIO $ renamePath input newpath
  where
    firstAvailableEnding i e =
      doesPathExist fileName >>= \case
      True -> firstAvailableEnding (i + 1) ("v" ++ show i)
      False -> return fileName
      where fileName = input <.> e
