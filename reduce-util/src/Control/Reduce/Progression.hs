{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE BlockArguments            #-}
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
Module      : Control.Reduce.Progression
Copyright   : (c) Christian Gram Kalhauge, 2020
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module defines how to calculate a progression.
-}
module Control.Reduce.Progression
  ( postOrderLogicalClosure
  , progression
  , runProgression
  ) where

-- base
import Control.Monad.ST as ST
import Data.Either
import Data.STRef as ST
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

-- lens
import Control.Lens

-- mtl
import Control.Monad.Reader

-- containers
import qualified Data.IntSet as IS

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- reduce-util
import Control.Reduce.Boolean.LiteralSet as LS
import Control.Reduce.Boolean
import Control.Reduce.Boolean.CNF hiding (progression)

data ProgressionState s = ProgressionState
  { progClauses :: VM.MVector s (Maybe Clause)
  , progPositiveClauses :: STRef s IS.IntSet
  , progReverseLookup :: V.Vector [Int]
  , progVisited :: VM.MVector s Bool
  }

type ProgressionM s = ReaderT (ProgressionState s) (ST s)

isPositive :: Clause -> Bool
isPositive clause = case LS.minView clause of
  Just (toLiteral -> Literal True _, _) -> True
  Nothing -> False

runProgression :: Int -> CNF -> (forall s. ProgressionM s a) -> a
runProgression numVars (CNF clauses) runM = runST $ do
  let vClauses = V.fromList . S.toList $ clauses
  pClauses <- V.thaw $ V.map Just vClauses
  visited <- VM.replicate numVars False
  positive <- newSTRef IS.empty
  clauseLookup <- VM.replicate numVars IS.empty

  iforM_ vClauses \i c -> do
    forM_ (IS.toList $ LS.variables c) do
      VM.modify clauseLookup (IS.insert i)

  cl <- V.freeze clauseLookup

  let
    initialState = ProgressionState
      { progClauses = pClauses
      , progPositiveClauses = positive
      , progReverseLookup = V.map IS.toList cl
      , progVisited = visited
      }

  flip runReaderT initialState do
    iforM_ vClauses \i c -> when (isPositive c) do
      addPositiveClause i
    runM


progression :: ProgressionM s (NE.NonEmpty [Int])
progression = do
  a <- postOrderLogicalClosure
  nv <- numberOfVariables
  (a NE.:|) <$> go 0 nv
 where
  go i nv
    | i < nv = isVisited i >>= \case
        True  -> go (i+1) nv
        False -> do
          conditionTrue i
          c <- postOrderLogicalClosure
          ((c ++ [ i ]) :) <$> go (i+1) nv
    | otherwise = pure []

numberOfVariables :: ProgressionM s Int
numberOfVariables = ReaderT $ pure . VM.length . progVisited

isVisited :: Int -> ProgressionM s Bool
isVisited i = ReaderT $ \p -> VM.read (progVisited p) i

markVisited :: Int -> ProgressionM s ()
markVisited i = ReaderT $ \p -> VM.write (progVisited p) i True

-- | The post-order logical closure visits the variables
-- in a set of clauses in post-order.
postOrderLogicalClosure :: ProgressionM s [Int]
postOrderLogicalClosure = reverse <$> go where
  go = nextPositiveVar >>= \case
    Just v -> do
      conditionTrue v
      (v:) <$> go
    Nothing ->
      return []

conditionTrue :: Int -> ProgressionM s ()
conditionTrue v = do
  mapM_ conditionClause =<< clausesOf v
  markVisited v
 where
  conditionClause :: Int -> ProgressionM s ()
  conditionClause cidx = updateClause cidx \case
    Just (LS.conditionClause (tt v) -> Just clause) -> do
      when (isPositive clause) $ addPositiveClause cidx
      return ((), Just clause)
    _ -> return ((), Nothing)

addPositiveClause :: Int -> ProgressionM s ()
addPositiveClause cidx = ReaderT \p ->
  ST.modifySTRef' (progPositiveClauses p) (IS.insert cidx)

updateClause ::
  Int
  -> (Maybe Clause -> ProgressionM s (a, Maybe Clause))
  -> ProgressionM s a
updateClause i fn = ReaderT \p -> do
  x <- VM.read (progClauses p) i
  (a, st) <- runReaderT (fn x) p
  VM.write (progClauses p) i st
  return a


clausesOf :: Int -> ProgressionM s [Int]
clausesOf v = ReaderT \p -> pure $ progReverseLookup p V.! v

nextPositiveVar :: ProgressionM s (Maybe Int)
nextPositiveVar = ReaderT \ProgressionState { .. } -> updateSTRef' progPositiveClauses \s -> do
  (partitionEithers -> (rm, itms)) <- forM (IS.toList s) $ \i -> do
    (firstVar <$> VM.read progClauses i) <&> \case
      Just v  -> Right v
      Nothing -> Left i
  return
    ( maybe Nothing (Just . minimum) (NE.nonEmpty itms)
    , IS.difference s (IS.fromList rm)
    )
 where
  firstVar mc = mc >>= fmap fst . IS.minView . LS.variables









