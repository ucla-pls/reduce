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
  (

  reduction
  , reductionWith
  , ReducerName (..)

  -- * Re-Exports

  , module Control.Reduce.Problem

  -- * Utils
  , findOutputFile
  , versionInputFile
  ) where

-- base
import           Control.Exception          (AsyncException (..))
import           Control.Monad
import           Text.Printf

-- unliftio
import           UnliftIO
import           UnliftIO.Directory

-- time
import           Data.Time

-- lens
import           Control.Lens hiding ((<.>))

-- filepath
import           System.FilePath

-- mtl
import           Control.Monad.Reader

-- reduce
import           Control.Reduce

-- -- containers
-- import qualified Data.IntSet as IS

-- -- vector
-- import qualified Data.Vector as V

-- reduce-util
import           Control.Reduce.Metric
-- import           Control.Reduce.Graph
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


-- graphReduction :: Monad m => [Edge () k] -> ReducerName -> Reducer m [k]
-- graphReduction edgs name pred items =
--   let
--     (graph, _) = buildGraphFromNodesAndEdges [ (i, i) | i <- items ] edgs
--     labs = nodeLabels graph
--     fn = fmap (labs V.!) . IS.toAscList . IS.unions
--   in intsetReduction name pred (closures graph)

-- -- | Do a reduction over an 'IntSet'
-- intsetReduction ::
--   Monad m =>
--   ReducerName
--   -> Reducer m [IS.IntSet]
-- intsetReduction name =
--   reduction (IS.size . IS.unions)

-- data ReductF a f
--   = Check a (Bool -> f)
--   deriving (Functor)

-- type ReductM x = F (ReductF x)

-- type Strategy a = a -> ReductM a (Maybe a)

-- data AbstractProblem a =
--   AbstractProblem (Strategy a) (Problem a)

-- check :: a -> ReductM a Bool
-- check a = liftF $ Check a id

-- -- | Do a reduction over a list
-- listReductM :: ([x] -> a) -> ReducerName -> [x] -> ReductM a (Maybe a)
-- listReductM c name lst =
--   fmap c <$> case name of
--     Ddmin  -> ddmin predc lst
--     Binary -> binaryReduction predc lst
--     Linear -> linearReduction predc lst
--   where
--     predc = PredicateM $ check . c


-- listReduction :: ReducerName -> Strategy [a]
-- listReduction = listReductM id

-- -- | Do a reduction over a list of sets
-- setReduction :: Ord x => ReducerName -> Strategy [S.Set x]
-- setReduction red xs =
--   case red of
--     Ddmin  -> ddmin predc sxs
--     Binary -> genericBinaryReduction (S.size . S.unions) predc xs
--     Linear -> linearReduction predc sxs
--   where
--     sxs = L.sortOn (S.size) xs
--     predc = PredicateM check

-- intsetReduction :: ReducerName -> Strategy [IS.IntSet]
-- intsetReduction = intsetReduct id

-- -- | Do a reduction over an 'IntSet'
-- intsetReduct :: ([IS.IntSet] -> a) -> ReducerName -> [IS.IntSet] -> ReductM a (Maybe a)
-- intsetReduct fn red xs =
--   fmap fn <$> case red of
--     Ddmin  -> ddmin predc sxs
--     Binary -> genericBinaryReduction (IS.size . IS.unions) predc xs
--     Linear -> linearReduction predc sxs
--   where
--     sxs = L.sortOn (IS.size) xs
--     predc = PredicateM (check . fn)

-- -- | Strategy for reducing trees
-- data TreeStrategy
--   = HddStrategy
--   | GraphStrategy
--   | FlatStrategy
--   deriving (Show, Read, Ord, Eq)

-- treeStrategy ::
--   [Edge () [Int]]
--   -> TreeStrategy
--   -> ReducerName
--   -> Strategy [[Int]]
-- treeStrategy edgs = \case
--     FlatStrategy -> listReduction
--     GraphStrategy -> graphReduction edgs
--     HddStrategy -> hddReduction


-- hddReduction :: ReducerName -> Strategy [[Int]]
-- hddReduction name = go 1
--   where
--     go :: Int -> Strategy [[Int]]
--     go n items
--       | length reductionLayer > 0 = do
--         listReductM back name (traceShowId reductionLayer) >>= \case
--           Just items' -> go (n+1) items'
--           Nothing -> return $ Just items
--      | otherwise =
--        return $ Just items
--         where
--           (reductionLayer, rest) = L.partition (\i -> length i == n) items
--           keep = S.fromList rest
--           back = S.toAscList . S.union keep . S.fromList

-- runAbstractProblem ::
--   ReductionOptions
--   -> FilePath
--   -> AbstractProblem b
--   -> L.Logger (Maybe ReductionException, b)
-- runAbstractProblem opts fp (AbstractProblem red problem) =
--   runReduction opts fp red problem


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
