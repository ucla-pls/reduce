{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Control.Reduce.Reducable
Description : A s
Copyright   : (c) Christian Kalhauge
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

-}
module Control.Reduce.Reducability where

-- base
import Data.Maybe
import Data.Monoid
import Data.Functor.Compose

-- vector
import qualified Data.Vector as V

-- unordered-containers
import qualified Data.HashMap.Strict as HM

-- lens
import Control.Lens

-- aeson
import Data.Aeson

-- mtl
import Control.Monad.State.Strict

-- | Reducability is a traversal that make the structure smaller, if some sub
-- structure stops existing.
type Reducability s b =
  Traversal s (Maybe s) b (Maybe b)

allR :: Traversal' s b -> Reducability s b
allR t fab s =
  getCompose (t (Compose . fab) s)

-- | A list is reducable.
sndR :: Reducability (a, b) b
sndR = allR _2

-- | A list is reducable.
fstR :: Reducability (a, b) a
fstR = allR _1

isoR :: (a -> b) -> (b -> a) -> Reducability a b
isoR ab ba = allR $ iso ab ba

-- | A list is reducable.
listReducable :: Reducability [a] a
listReducable afb =
  fmap (Just . catMaybes) . traverse afb

-- | JSON is reducable
jsonReducable :: Reducability Value Value
jsonReducable afb = \case
  Array a -> fmap Array <$> (isoR V.toList V.fromList . listReducable) afb a
  String t -> pure $ Just (String t)
  Number s -> pure $ Just (Number s)
  Bool b -> pure $ Just (Bool b)
  Null -> pure $ Just Null
  Object o -> fmap Object <$> (isoR HM.toList HM.fromList . listReducable . sndR) afb o

-- | Count number of elements available for reduction.
countElements :: Reducability a b -> a -> Int
countElements red =
  getSum . getConst . red (const $ Const (Sum 1))

-- | Limit the number of elements in a reducable structure.
limitElements :: (Int -> Bool) -> Reducability a b -> a -> (Maybe a)
limitElements keep red =
  flip evalState 0 . red fm
  where
   fm :: a -> State Int (Maybe a)
   fm e = do
     x <- state (\i -> (i, i+1))
     if keep x
       then return $ Just e
       else return $ Nothing

-- | Choose a reduction level. Usefull for doing Hierarchical Delta Debugging.
chooseReductionLevel :: Reducability a a -> Int -> Reducability a a
chooseReductionLevel red = go
  where
    go 0 = red
    go n = red . go (n - 1)

-- | Count elements at level n.
deepCountElements :: Reducability a a -> Int -> a -> Int
deepCountElements red n =
  countElements (chooseReductionLevel red n)

-- | Limit elements at level n
deepLimitElements :: (Int -> Bool) -> Reducability a a -> Int -> a -> (Maybe a)
deepLimitElements keep red n =
  limitElements keep (chooseReductionLevel red n)
