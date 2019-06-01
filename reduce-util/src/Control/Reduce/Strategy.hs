{-# LANGUAGE DeriveFunctor #-}
{-|
Module      : Control.Reduce.Strategy
Description : A module to talk about reduction strategies
Copyright   : (c) Christian Kalhauge <kalhauge@cs.ucla.edu>
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

A module to talk about reduction strategies
-}
module Control.Reduce.Strategy where

-- free
import           Control.Monad.Free.Church

-- | The name of the reducer
data ReducerName
  = Ddmin
  | Linear
  | Binary
  deriving (Show, Read, Ord, Eq)

-- | Strategy for reducing trees
data TreeStrategy
  = HddStrategy
  | GraphStrategy
  | FlatStrategy
  deriving (Show, Read, Ord, Eq)

data ReductF a f
  = Check a (Bool -> f)
  deriving (Functor)

type ReductM x = F (ReductF x)

newtype Strategy a = Strategy
  { runStrategy
    :: ReducerName
    -> TreeStrategy
    -> a
    -> ReductM a (Maybe a)
  }
