{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Control.Reduce.Reduction
Description : A s
Copyright   : (c) Christian Kalhauge
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

-}
module Control.Reduce.Reduction
  (
    -- * Traversals
    Reduction
    , SafeReduction
    , Reduct
    , DeepReduction

    -- ** Constructors
    , allOrNothing
    , adventure

    -- ** Accessing
    , subelements
    , deepsubelements
    , indicesOf
    , getting

    -- * Algorithms
    , limit
    , limiting

    , deepening
    , boundedDeepening

    -- * Implementations
    , listR
    , fstR
    , sndR
    , vectorR
    , hashmapR
    , jsonR
    , treeR

  ) where

-- base
import Data.Maybe
import Data.Functor.Compose

-- unordered-containers
import qualified Data.HashMap.Strict as HM

-- containers
import qualified Data.Tree as T

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


-- | A 'Reduct' is the most general reduction.
type Reduct p s t a =
  forall f. Applicative f => Over p f s t a (Maybe a)

-- | A 'Reduction' is a traversal of an `s` that can maybe can return a smaller
-- `s`, if we map a collection of elements to maybe existing or not. This traversal
-- can also change the content of the structure, but not the type.
-- A 'Reduction' composes with other reductions.
type Reduction s a = Reduct (->) s (Maybe s) a

-- | A 'SafeReduction' is like a reduction but can handle failure in it's own
-- data structure. SafeReduction can be composed with traversals on the left
-- and normal reductions on the right.
-- @
-- (a :: Traversal s t) . (b :: SafeReduction s b) :: SafeReduction t b
-- @
type SafeReduction s a = Reduct (->) s s a

-- | Forget the safety of the 'SafeReduction'
adventure :: SafeReduction s a -> Reduction s a
adventure sred fa = fmap Just . sred fa
{-# INLINE adventure #-}

-- | Get the possible elements of a reduction
subelements :: Reduct (->) s t a -> IndexedFold Int s a
subelements red =
  getting (indexing red)
{-# INLINE subelements #-}

-- | Get the all the recursive deep subelements.
deepsubelements :: Reduction s s -> IndexedFold [Int] s s
deepsubelements red =
  getting (deepening red)
{-# INLINE deepsubelements #-}

-- | The trivial reduction, if any sub element is removed then the structure
-- cannot be constructed. This can be constructed for all elements.
allOrNothing :: Traversal' s a -> Reduction s a
allOrNothing t fab s =
  getCompose (t (Compose . fab) s)
{-# INLINE allOrNothing #-}


-- | Get the indices of a reduction.
indicesOf :: Reduct (Indexed i) s t a -> s -> [i]
indicesOf p =
  map fst . itoListOf (getting p)

-- | limit
limit :: Reduct (Indexed i) s t a -> (i -> Bool) -> s -> t
limit red keep =
  iover red (\i a -> if keep i then Just a else Nothing)
{-# INLINE limit #-}

-- | Given and 'Reduction' and a function indicating which elements to keep
-- remove those elements.
limiting :: Reduct (->) s t a -> (Int -> Bool) -> s -> t
limiting red = limit (indexing red)
{-# INLINE limiting #-}

type DeepReduction s =
  forall p. Indexable [Int] p => Reduct p s (Maybe s) s

-- | Create a deep reduction from a reduction to itself. The first argument
-- is the maximum depth.
boundedDeepening ::
  Int
  -> Reduction s s
  -> DeepReduction s
boundedDeepening n red pab =
  go n []
  where
    red' = indexing red
    go 0 fi a = indexed pab fi a
    go n' fi a =
      pure (*>)
      <*> indexed pab fi a
      <*> red' (Indexed $ \x -> go (n' - 1) (x:fi)) a
{-# INLINE boundedDeepening #-}

-- | Like 'boundedDeepening' but has no bound.
deepening :: Reduction a a -> DeepReduction a
deepening = boundedDeepening (-1)
{-# INLINE deepening #-}

-- -- | Like limitedDeepR, but with no max depth.
-- deepR :: Reduction a a -> DeepReduction a
-- deepR = boundedDeepR (-1)

-- -- | 'DeepReduction' is reduction over a structure that have itself embedded in it.
-- type DeepReduction s =
--   IndexedReduction [Int] s s

-- * Implementations

-- | A list is reducable.
listR :: SafeReduction [a] a
listR pab =
  fmap catMaybes . itraverse (indexed pab)

-- | We can reduce the first element in a tuple
fstR :: Reduction (a, b) a
fstR = allOrNothing _1

-- | We can reduce the second element in a tuple
sndR :: Reduction (a, b) b
sndR = allOrNothing _2

-- | We can reduce a 'Vector' by turning it into a list and back again.
vectorR :: SafeReduction (V.Vector b) b
vectorR = iso V.toList V.fromList . listR

-- | We can reduce a 'HM.HashMap' by turning it into a list and back again.
hashmapR :: (HM.Hashable a, Eq a) => SafeReduction (HM.HashMap a b) b
hashmapR = iso HM.toList HM.fromList . listR . sndR

-- | JSON is reducable
jsonR :: SafeReduction Value Value
jsonR afb = \case
  Array a -> Array <$> vectorR afb a
  String t -> pure $ String t
  Number s -> pure $ Number s
  Bool b -> pure $ Bool b
  Null -> pure $ Null
  Object o -> Object <$> hashmapR afb o

-- | A 'T.Tree' is reducable
treeR :: SafeReduction (T.Tree a) (T.Tree a)
treeR afb = \case
  T.Node a f ->
    T.Node a <$> listR afb f
