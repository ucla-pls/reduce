{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Control.Reduce.Reduction
Description : A s
Copyright   : (c) Christian Kalhauge
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

-}
module Control.Reduce.Reduction where

-- base
import Data.Maybe
import Data.Functor.Compose

-- unordered-containers
import qualified Data.HashMap.Strict as HM

-- hashable
import qualified Data.Hashable as HM

-- vector
import qualified Data.Vector as V

-- lens
import Control.Lens

-- aeson
import Data.Aeson

-- -- mtl
-- import Control.Monad.State.Strict

-- | A 'Reduction' is a traversal of an `s` that can maybe can return a smaller
-- `s`, if we map a collection of elements to maybe existing or not. This traversal
-- can also change the content of the structure, but not the type.
type Reduction s a =
  Traversal s (Maybe s) a (Maybe a)

-- | The trivial reduction, if any sub element is removed then the structure
-- cannot be constructed. This can be constructed for all elements.
allR :: Traversal' s a -> Reduction s a
allR t fab s = getCompose (t (Compose . fab) s)
{-# INLINE allR #-}

-- | The reduction traversal does not compose nicely with the getter part of the
-- lens library.
getR :: Reduction s a -> Traversal' (Maybe s) a
getR red fa = \case
  Just a -> red (fmap Just . fa) a
  Nothing -> pure Nothing
{-# INLINE getR #-}

foldR :: Reduction s a -> Fold s a
foldR red fa s =
  Just `contramap` red (fmap Just . fa) s
{-# INLINE foldR #-}

-- | Return a list of subelements.
subelementsR :: Reduction s a -> s -> [a]
subelementsR red =
  toListOf (foldR red)
{-# INLINE subelementsR #-}

-- | IndexedReduction, mostly left here for ease of use.
type IndexedReduction i s a =
  IndexedTraversal i s (Maybe s) a (Maybe a)

-- | 'indexingR' is just 'indexing', and have mostly been defined
-- for ease of use.
indexingR :: Reduction s a -> IndexedReduction Int s a
indexingR = indexing
{-# INLINE indexingR #-}

-- | Turn an Reduc into a IndexedFold
ifoldR ::
  IndexedReduction i s a
  -> IndexedFold i s a
ifoldR red fa s =
  Just `contramap` red (rmap (fmap Just) fa) s
{-# INLINE ifoldR #-}

-- getIR :: Reduction s a -> IndexedTraversal' Int (Maybe s) a
-- getIR red fa =
--   \case
--     Nothing -> pure Nothing
--     Just s -> indexing red (lmap (fmap Just) fa) s
-- {-# INLINE getIR #-}

-- | limit
limitR :: (i -> Bool) -> IndexedReduction i s a -> s -> Maybe s
limitR keep red s =
  runIdentity $ itraverseOf red
  (\i a -> pure $ if keep i then Just a else Nothing) s

-- | count
countR :: Reduction s a -> s -> Int
countR red = sumOf (foldR red . like 1)

-- | count
indiciesR :: IndexedReduction i s a -> s -> [i]
indiciesR red = map fst . itoListOf (ifoldR red)

-- | 'DeepReduction' is reduction over a structure that have itself embedded in it.
type DeepReduction s =
  IndexedReduction [Int] s s

-- | Create a deep reduction from a reduction to itself. The first argument
-- is the maximum depth.
limitedDeepR :: Int -> Reduction a a -> DeepReduction a
limitedDeepR n red pab =
  go n id
  where
    --  forall p f. (Indexable [Int] p, Applicative f) => s -> f (Maybe s)
    go 0 _ a = pure (Just a)
    go n' fi a =
      pure (*>)
      <*> indexed pab (fi []) a
      <*> indexingR red (Indexed $ \x -> go (n' - 1) (fi . (x:)) ) a
{-# INLINE limitedDeepR #-}

-- | Like limitedDeepR, but with no max depth.
deepR :: Reduction a a -> DeepReduction a
deepR = limitedDeepR (-1)

-- * Implementations

-- | A list is reducable.
listR :: Reduction [a] a
listR pab =
  fmap (Just . catMaybes) . itraverse (indexed pab)

-- | We can reduce the first element in a tuple
fstR :: Reduction (a, b) a
fstR = allR _1

-- | We can reduce the second element in a tuple
sndR :: Reduction (a, b) b
sndR = allR _2

-- | Anything that is isomorphic is alos able to be reduced.
isoR :: (a -> b) -> (b -> a) -> Reduction a b
isoR ab ba = allR $ iso ab ba

-- | We can reduce a 'Vector' by turning it into a list and back again.
vectorR :: Reduction (V.Vector b) b
vectorR = isoR V.toList V.fromList . listR

-- | We can reduce a 'HM.HashMap' by turning it into a list and back again.
hashmapR :: (HM.Hashable a, Eq a) => Reduction (HM.HashMap a b) b
hashmapR = isoR HM.toList HM.fromList . listR . sndR

-- | JSON is reducable
jsonR :: Reduction Value Value
jsonR afb = \case
  Array a -> fmap Array <$> (isoR V.toList V.fromList . listR) afb a
  String t -> pure $ Just (String t)
  Number s -> pure $ Just (Number s)
  Bool b -> pure $ Just (Bool b)
  Null -> pure $ Just Null
  Object o -> fmap Object <$> (isoR HM.toList HM.fromList . listR . sndR) afb o



-- -- | The trivial reduction, if any sub element is removed then the structure
-- -- cannot be constructed. This can be constructed for all elements.
-- drop :: Traversal' s b -> Reduction s b
-- allR t fab s = getCompose (t (Compose . fab) s)

-- justR :: Traversal (Maybe a) (Maybe a) a (Maybe a)
-- justR fab s =
--   case s of
--     Just a -> fab a
--     Nothing -> pure Nothing




-- -- | Count number of elements available for reduction.
-- countElements :: Reducability a b -> a -> Int
-- countElements red =
--   getSum . getConst . red (const $ Const (Sum 1))

-- -- | Limit the number of elements in a reducable structure.
-- limitElements :: (Int -> Bool) -> Reducability a b -> a -> (Maybe a)
-- limitElements keep red =
--   flip evalState 0 . red fm
--   where
--    fm :: a -> State Int (Maybe a)
--    fm e = do
--      x <- state (\i -> (i, i+1))
--      if keep x
--        then return $ Just e
--        else return $ Nothing


-- type IReducability s a =
--   IndexedTraversal Int s (Maybe s) a (Maybe a)

-- type DeepReducability s =
--   IndexedTraversal [Int] s (Maybe s) s (Maybe s)


-- -- | Choose a reduction level. Usefull for doing Hierarchical Delta Debugging.
-- chooseReductionLevel :: Reducability a a -> Int -> Reducability a a
-- chooseReductionLevel red = go
--   where
--     go 0 = red
--     go n = red . go (n - 1)

-- -- | Count elements at level n.
-- deepCountElements :: Reducability a a -> Int -> a -> Int
-- deepCountElements red n =
--   countElements (chooseReductionLevel red n)

-- -- | Limit elements at level n
-- deepLimitElements :: (Int -> Bool) -> Reducability a a -> Int -> a -> (Maybe a)
-- deepLimitElements keep red n =
--   limitElements keep (chooseReductionLevel red n)
