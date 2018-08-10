{-# LANGUAGE ViewPatterns #-}
{-|
Module      : Control.Reduce
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module contains functions to reduce a set to a smaller set given a
predicate.

8|-}
module Control.Reduce where

import           Control.Applicative

import           Control.Monad
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

import           Data.Functor
import           Data.Monoid

import qualified Data.IntSet as IS
import qualified Data.Vector as V
import qualified Data.List as L

import Debug.Trace

-- * Reducers

-- | A predicate is a function from a list of elements to
-- a monadic action that either succeeds or fails.
type Predicate e m =
  e -> m Bool

-- | A 'Reducer' is now a function that takes a list of elements and returns
-- the smallest list of elements such that the predicate is still satisfied.
-- The reducer will return nothing if no subset satisfies the predicate.
type Reducer s m =
  Predicate s m -> s -> m (Maybe s)


-- ** Delta Debugging
-- Delta debugging as per Zeller and Hildebrandt 2002.
--
--  Zeller, Andreas, and Ralf Hildebrandt. "Simplifying and isolating
--  failure-inducing input." IEEE Transactions on Software Engineering 28, no. 2
--  (2002): 183-200.

-- | An easy to call interface.
ddmin :: Monad m => Reducer [e] m
ddmin p es = do
  t' <- p []
  if t'
    then return $ Just []
    else do
      mx <- liftReducer (ddmin' 2) p es
      case mx of
        Just x -> do
          t <- p x
          return $ if t then Just x else Nothing
        Nothing -> return Nothing

-- | The implementation of delta debugging.
ddmin' :: Monad m => Int -> IReducer m
ddmin' n test world =
  cases
    [ guard (L.length deltas <= 1) $> world
    , cases [ testrec 2 d | d <- deltas ]
    , guard (n > 2) >>
      cases [ testrec (max (n - 1) 2) (world IS.\\ d) | d <- deltas ]
    , guard (n < size) >> ddmin' (min size (2*n)) test world
    , return world
    ]
  where
    testrec n d = test d >> ddmin' n test d
    deltas = splitSet n world
    size = IS.size world

-- ** Binary Reduction

-- | Linary reduction is just going through the list by hand and removing one
-- element at a time
linaryReduction :: Monad m => Reducer [e] m
linaryReduction p xs =
  runMaybeT (pred xs >> go [] xs)
  where
    pred = liftPredicate p
    go !sol !es =
      case es of
        [] -> return sol
        e:es' -> do
          sol' <- ($ sol) <$> cases [ pred (sol ++ es') $> id, pure (++ [e])]
          go sol' es'


-- | Binary reduction is the simplest form of the set minimizing algorithm.
-- As such it is fast, while still having the 1-minimality property.
binaryReduction :: Monad m => Reducer [e] m
binaryReduction p es =
  runMaybeT . go [] $ L.length es
  where
    pred = liftPredicate p
    go !sol !n = do
      r <- binarySearch (pred . range) 0 n
      cases
        [ guard (r > 0) >> go (es L.!! (r - 1) : sol) (r - 1)
        , return $ range r
        ]
      where range i = L.take i es ++ sol

binaryReductions :: (Show e, Eq e, Monad m) => Predicate [e] m -> [e] -> m [[e]]
binaryReductions p =
  fmap (map (L.reverse)) . go []
  where
    pred = liftPredicate p
    go !sol !xs = do
      mr <- runMaybeT $ binarySearch (pred . range) 0 (L.length xs)
      case mr of
        Just r
          | r > 0 -> do
              let e = xs L.!! (r - 1)
              rs <- go  (e : sol) (L.take (r-1) xs)
              case rs of
                (m:_) ->
                  (map (e:) rs ++) . concat <$> mapM (go sol . flip L.delete xs) m
                [] ->
                  return []
          | otherwise ->
            return [[]]
        Nothing ->
          return []
      where range i = L.take i xs ++ sol


-- * Generic Reducers
-- A set reducer is able to take a list of sets, and then produce a
-- smaller combined set such that a predicate is still unhold.
--
-- Set reducers only makes sense when the sets are overlapping or if there are
-- multiple minima. In that case there is a big difference between
-- returning a single set of size N or 2 sets of size 1. We want to strive to
-- return as small final set as possible.
--
-- TODO: Find a definition that we want to achieve.

genericBinaryReduction :: (Monad m, Show a) => ([a] -> Int) -> Reducer [a] m
genericBinaryReduction cost (liftPredicate -> pred) =
  runMaybeT . go []
  where
    go !sol (L.sortOn (cost . (:sol)) -> !as) = do
      r <- binarySearch (pred . (\i -> L.take i as ++ sol)) 0 (L.length as)
      if r > 0
        then do
          let (as', rs:ys) = L.splitAt (r - 1) as
          go (rs:sol) as' <|> (return $ as' ++ rs:sol)
        else
          return sol

generic2BinaryReduction :: Monad m => ([a] -> Int) -> Reducer [a] m
generic2BinaryReduction cost p es  =
  runMaybeT . go (cost es) [] $ es
  where
    pred = liftPredicate p
    go k !sol as' = do
      guard (cost sol <= k)
      r <- binarySearch (pred . range) 0 (L.length as)
      if (r > 0)
        then do
          let
            (as', rs:ys) = L.splitAt (r - 1) as
            sol' = rs:sol
          cases
            [ pred sol' >> return sol'
            , do
              x <- go k sol' as'
              go (cost x - 1) sol (as' ++ ys) <|> return x
            ]
        else
          return sol
      where
        range i = L.take i as ++ sol
        as =
          L.map snd
          . L.sortOn fst
          . L.filter ((<= k) . fst)
          . L.map (\e -> (cost (e:sol), e))
          $ as'


-- | An 'ISetReducer' like a generic reducer but uses slightly optimized
-- data-structures.
type ISetReducer m =
  Predicate IS.IntSet m -> [IS.IntSet] -> m (Maybe [IS.IntSet])

-- | SetBinaryReduction is much like regular binary reduction, but with
-- one added feature. Since we want the smallest output set we have to
-- continuously sort the list of set in size  to get the smallest
-- possible set.
setBinaryReduction :: Monad m => ISetReducer m
setBinaryReduction (liftPredicate -> pred) = do
  runMaybeT . go ([], IS.empty) . map (\a -> (a, a))
  where
    go !(sol, h) (L.sortOn (IS.size . snd) -> !as) = do
      let u = V.fromList $ L.scanl (\a -> IS.union a . snd) h as
      r <- binarySearch (pred . V.unsafeIndex u) 0 (V.length u - 1)
      if (r > 0)
        then do
          let
            (as', (rs, ru):_) = L.splitAt (r - 1) as
            h' = IS.union h ru
          cases
            [ pred h' >> return (rs:sol)
            , go (rs:sol, IS.union h ru)
                [(a, s') | (a, s) <- as', let s' = s IS.\\ ru, not (IS.null s')]
                <|> (return $ map fst as' ++ rs:sol)
            ]
        else
          return sol

-- * Utilities

-- | binarySearch, returns a number between lw and hg that satisfies
-- the predicate. It requires that if p is monotone.
-- $p n = true then p n-1 = true$. fails if no such number exists.
binarySearch :: (MonadPlus m) => MPredicate Int m -> Int -> Int -> m Int
binarySearch p !lw !hg = do
  let pivot = lw + ((hg - lw) `quot` 2)
  cases
    [ p pivot
      >> if lw == pivot
          then return lw
          else binarySearch p lw pivot
    , guard (pivot < hg)
      >> binarySearch p (pivot + 1) hg
    ]

-- | Splits a set into at most n almost same sized sets.
--
-- prop> s = IS.unions $ splitSet n s
splitSet :: Int -> IS.IntSet -> [IS.IntSet]
splitSet n s =
  [ IS.fromAscList p
  | p <- partition (IS.size s `roundUpQuot` n) $ IS.toAscList s
  ]
  where
    roundUpQuot i j =
      q + if r > 0 then 1 else 0
      where (q, r) = quotRem i j

    partition n [] = []
    partition n s =
      h : partition n r
      where (h, r) = splitAt n s

-- ** MonadPlus related

-- | Return a value only if some requirement is uphold
onlyif :: MonadPlus m => a -> (a -> Bool) -> m a
onlyif a p = guard (p a) $> a

-- | Return a value only if some requirement is uphold
onlyifM :: MonadPlus m => a -> (a -> m ()) -> m a
onlyifM a p = p a $> a

-- | Given a list of cases, return the result of the first succeding
-- computation.
cases :: MonadPlus m => [m a] -> m a
cases = msum

-- ** Conversion

-- | An predicate is like a ''Predicate', but the success of the predicate is
-- now encoded in the monad itself.
type MPredicate e m =
  e -> m ()

-- | Like a 'Reducer', but uses an 'IS.IntSet' instead for performance gain.
type IReducer m =
  MPredicate IS.IntSet (MaybeT m) -> IS.IntSet -> MaybeT m IS.IntSet

-- | Transform a Predicate to a MPredicate
liftPredicate :: Monad m => Predicate e m -> MPredicate e (MaybeT m)
liftPredicate pred =
  lift . pred >=> guard

-- | Transform a IReducer to a Reducer
liftReducer :: Monad m => IReducer m -> Reducer [e] m
liftReducer red pred es = do
  mr <- runMaybeT $ red (liftPredicate pred . unset) world
  return $ unset <$> mr
  where
    refs = V.fromList es
    world = IS.fromAscList [0..(V.length refs - 1)]
    unset = map (V.unsafeIndex refs) . IS.toAscList

-- | Transform a ISetReducer to a Reducer
toSetReducer :: Monad m => Reducer [IS.IntSet] m -> ISetReducer m
toSetReducer red pred es = do
  red (pred . IS.unions) es

-- | Transform a ISetReducer to a Reducer
liftISetReducer :: Monad m => ISetReducer m -> Reducer [a] m
liftISetReducer red pred es = do
  mr <- red (pred . unset) world
  return $ unset . IS.unions <$> mr
  where
    refs = V.fromList es
    world = [IS.singleton i | i <- [0..(V.length refs - 1)]]
    unset = map (V.unsafeIndex refs) . IS.toAscList
