{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-|
Module      : Control.Reduce.Reduction
Description : A module of reducable things
Copyright   : (c) Christian Kalhauge
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

Everything that is reducable have a 'Reduction'. A reduction is a traversal over
an element to internal elements which maybe not exist after reduction.
-}
module Control.Reduce.Reduction
  (
    Reduction

    , PartialReduction
    , part
    , all'
    , orNothing
    , subelements
    , reduceAs

    -- * Algorithms
    , Reduct
    , indicesOf
    , getting
    , limit
    , limiting

    -- * Recursive Reduction

    -- ** Tree Reduction
    , TreeReduction
    , treeReduction
    , treeSubelements

    -- ** Deep Reduction
    , DeepReduction
    , deepReduction
    , boundedDeepening
    , deepSubelements

    -- * Implementations
    , maybeR
    , listR
    , atleastoneR
    , vectorR
    , hashmapR
    , jsonR
    , treeR
    , dirTreeR
    , dirForestR
    , deepDirTreeR
    , deepDirForestR
  ) where

-- base
import           Data.Functor
import           Data.Functor.Compose
import qualified Data.List.NonEmpty   as NE
import           Data.Maybe

-- unordered-containers
import qualified Data.HashMap.Strict  as HM

-- containers
import qualified Data.Tree            as T

-- hashable
import qualified Data.Hashable        as HM

-- vector
import qualified Data.Vector          as V

-- dirtree
import           System.DirTree

-- lens
import           Control.Lens

-- aeson
import           Data.Aeson

-- | A 'Reduct' is the most general reduction.
type Reduct p s t a =
  forall f. Applicative f => Over p f s t a (Maybe a)

-- | A 'Reduction' is a traversal over a @s@ to
-- So a basic traversal can be written like this:
--
-- @
-- Traversal' s a ~ (a -f-> a) -> (s -f-> s)
-- @
--
-- A reduction is a little different:
--
-- @
-- Reduction s a ~ (a -f-> Maybe a) -> (s -f-> s)
-- @
-- The reduction composes on the left with simple traversals
--
-- @
-- (f :: Traversal' a b ) . (g :: Reduction b c) :: Reduction a c
-- @
type Reduction s a = Reduct (->) s s a


{-|
A 'Reduction' composes on the right with 'PartialReduction's.
A 'PartialReduction' is something that might not be there after
a reduction.

@
'PartialReduction' s a ~ (a -f-> 'Maybe' a) -> (s -f-> 'Maybe' s)
@

'PartialReduction's form a category, since they compose and the identity is
trivially defined using @id@, but more importantly a 'PartialReduction' composes
with a 'Reduction' on the right.

@
(f :: Reduction a b ) . (g :: PartialReduction b c) :: Reduction a c
@
-}
type PartialReduction s a =
  forall f. Applicative f => Over (->) f s (Maybe s) a (Maybe a)

-- type IndexedReduction s a =
--   forall f p. (Applicative f, Indexable Int p) => Over p (Compose f Maybe) s s a a

-- type TestReduction s a =
--   forall f. Applicative f => Over (->) (Compose f Maybe) s s a a

-- example :: TestReduction a b -> Traversal' b c -> TestReduction a c
-- example a b = a . b

-- compare :: Traversal' a b -> TestReduction b c -> TestReduction a c
-- compare a b = a . b

-- indicies :: TestReduction a b -> IndexedReduction a b
-- indicies = decompose indexing

-- decompose ::
--   forall p p' f f' s a.
--   (Profunctor p', Profunctor p)
--   => (Over p f s (Maybe s) a (Maybe a) -> Over p' f' s (Maybe s) a (Maybe a))
--   -> (Over p (Compose f Maybe) s s a a -> Over p' (Compose f' Maybe) s s a a)
-- decompose x = recompose . x . uncompose

-- uncompose :: Profunctor p => Over p (Compose f Maybe) a a b b -> Over p f a (Maybe a) b (Maybe b)
-- uncompose t pab x =
--   getCompose . t (rmap Compose pab) $ x

-- recompose :: Profunctor p => Over p f a (Maybe a) b (Maybe b) -> Over p (Compose f Maybe) a a b b
-- recompose t pab x =
--   Compose . t (rmap getCompose pab) $ x


-- | Any 'Reduction' can be made into a 'PartialReduction' by just
-- returning Just.
part :: Reduction s a -> PartialReduction s a
part sred = part' . sred
{-# INLINE part #-}

part' :: Traversal s (Maybe t) s t
part' = fmap (fmap Just)
{-# INLINE part' #-}

-- | A 'Traversal' can be used like 'PartialReduction', where all subelements
-- are needed.
--
-- @
-- (f :: Reduction a b) . all' (g :: Traversal b c) :: Reduction a c
-- @
all' :: Traversal' s a -> PartialReduction s a
all' t fab = getCompose . t (Compose . fab)
{-# INLINE all' #-}

-- | A 'PartialReduction' can also be cast to a 'Reduction' based on a 'Maybe' of
-- the same type. We can do this by composing the partial reduction on the
-- right of 'maybeR' the reduction of maybe.
orNothing :: PartialReduction s a -> Reduction (Maybe s) a
orNothing t = maybeR . t
{-# INLINE orNothing #-}


{-| A 'DeepReduction' is applying either a partial or full reduction on itself,
recursively.

Essentially there exits two kinds of deep reductions. A DeepReduction based on a
full reduction does not propagate failure. Think about reducing a tree. If all
the subnodes of a tree is removed, it does not change the validity of the tree.
On contrary we can also reduce a 'PartialReduction' in which case the validity
of a node can depend on the existence of the subnodes. One example is (1 + 2), if
we remove any of the numbers we get an undefined expression.
-}
type TreeReduction s =
  forall p. Indexable (NE.NonEmpty Int) p => Reduct p s s s

-- treeReduct ::
-- (a -> f (Maybe a)) -> (a -> f a)
-- (p a (f (Maybe a)) -> a -> f a
treeReduction :: Reduction a a -> TreeReduction a
treeReduction red pab = go []
  where
    go !lst = indexing red (Indexed fn)
      where
        fn idx a =
          ($>)
          <$> indexed pab (idx NE.:| lst) a
          <*> go (idx : lst) a
{-# INLINE treeReduction #-}


type DeepReduction s =
  forall p. Indexable [Int] p => Reduct p s (Maybe s) s

-- deepReduct ::
-- (a -> f (Maybe a)) -> (a -> f (Maybe a))
-- (p a (f (Maybe a))) -> a -> f (Maybe a)
-- (p a (f . Maybe) a) -> a -> (f . Maybe) a
-- | Create a deep reduction from a reduction to itself. The first argument
-- is the maximum depth.
boundedDeepening ::
  Int
  -> Reduct (->) s (Maybe s) s
  -> DeepReduction s
boundedDeepening n red pab =
  go n []
  where
    red' = indexing red
    go 0 fi a = indexed pab fi a
    go n' fi a =
      (*>)
      <$> indexed pab fi a
      <*> red' (Indexed $ \x -> go (n' - 1) (x:fi)) a
{-# INLINE boundedDeepening #-}

-- | Like 'boundedDeepening' but has no bound.
deepReduction :: PartialReduction a a -> DeepReduction a
deepReduction = boundedDeepening (-1)
{-# INLINE deepReduction #-}

-- | Get the possible elements of a reduction
subelements :: Reduct (->) s t a -> IndexedFold Int s a
subelements red =
  getting (indexing red)
{-# INLINE subelements #-}

-- | Get the indices of a reduction.
indicesOf :: Reduct (Indexed i) s t a -> s -> [i]
indicesOf p =
  map fst . itoListOf (getting p)

-- | Limit any indexed reduction
limit :: Reduct (Indexed i) s t a -> (i -> Bool) -> s -> t
limit red keep =
  iover red (\i a -> if keep i then Just a else Nothing)
{-# INLINE limit #-}

-- | Given and 'Reduction' and a function indicating which elements to keep
-- remove those elements.
limiting :: Reduct (->) s t a -> (Int -> Bool) -> s -> t
limiting red = limit (indexing red)
{-# INLINE limiting #-}

-- | Get the all the recursive deep subelements.
deepSubelements :: PartialReduction s s -> IndexedFold [Int] s s
deepSubelements red =
  getting (deepReduction red)
{-# INLINE deepSubelements #-}

-- | Get the all the recursive deep subelements.
treeSubelements :: Reduction s s -> IndexedFold (NE.NonEmpty Int) s s
treeSubelements red =
  getting (treeReduction red)
{-# INLINE treeSubelements #-}

reduceAs :: Prism' a b -> PartialReduction b a
reduceAs p f a =
  f (review p a) <&> \case
    Just b -> b ^? p
    Nothing -> Nothing
{-# INLINE reduceAs #-}


-- * Implementations

-- | A list is reducable.
listR :: Reduction [a] a
listR pab =
  fmap catMaybes . itraverse (indexed pab)

-- | Like 'listR' but removes the list if empty
atleastoneR :: PartialReduction [a] a
atleastoneR f t =
  listR f t <&> \case
  [] -> Nothing
  a  -> Just a

-- | A 'Maybe' is trivially reducable.
maybeR :: Reduction (Maybe s) s
maybeR fab s =
  case s of
    Just s' -> fab s'
    Nothing -> pure Nothing

-- | We can reduce a 'Vector' by turning it into a list and back again.
vectorR :: Reduction (V.Vector b) b
vectorR = iso V.toList V.fromList . listR

-- | We can reduce a 'HM.HashMap' by turning it into a list and back again.
hashmapR :: (HM.Hashable a, Eq a) => Reduction (HM.HashMap a b) (a, b)
hashmapR = iso HM.toList HM.fromList . listR

-- | JSON is reducable
jsonR :: Reduction Value Value
jsonR afb = \case
  Array a -> Array <$> vectorR afb a
  String t -> pure $ String t
  Number s -> pure $ Number s
  Bool b -> pure $ Bool b
  Null -> pure Null
  Object o -> Object <$> (hashmapR . all' _2) afb o

-- | A 'T.Tree' is reducable
treeR :: Reduction (T.Tree a) (T.Tree a)
treeR afb = \case
  T.Node a f ->
    T.Node a <$> listR afb f

-- | A 'DirTree' is reducable
dirTreeR :: Reduction (DirTree a) (DirTree a)
dirTreeR fn n =
  DirTree <$> case dirTreeNode n of
    File b -> pure $ File b
    Directory b -> Directory <$> dirForestR fn b

-- | A 'DirForest' is reducable
dirForestR :: Reduction (DirForest a) (DirTree a)
dirForestR fn (DirForest b) =
  DirForest <$> (iso toFileList fromFileList . listR . all' _2) fn b

-- | Reduces the files in a dirtree. Removes directories if empty.
deepDirTreeR :: PartialReduction (DirTree a) a
deepDirTreeR f (DirTree tree) = case tree of
  File a ->
    fmap file <$> f a
  Directory b ->
    fmap directory <$> deepDirForestR f b

-- | Reduces the files in a DirForest. Removes directories if empty.
deepDirForestR :: PartialReduction (DirForest a) a
deepDirForestR f (DirForest a) =
  fmap DirForest <$>
    ( all' (iso toFileList fromFileList)
     . atleastoneR
     . all' _2
     . deepDirTreeR
    ) f a
