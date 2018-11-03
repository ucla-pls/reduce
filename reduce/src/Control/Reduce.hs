{-|
Module      : Control.Reduce
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module defines a set of reducers. A reducer is a function that
given a predicate reduces a set of items to a smaller set of items.

-}
module Control.Reduce
  ( Predicate
  , Reducer

  -- ** Delta Debugging
  -- | Delta debugging as per Zeller and Hildebrandt 2002.
  --
  --  Zeller, Andreas, and Ralf Hildebrandt. "Simplifying and isolating
  --  failure-inducing input." IEEE Transactions on Software Engineering 28, no. 2
  --  (2002): 183-200.
  , ddmin
  , unsafeDdmin

  -- ** Linear Reduction
  , linearReduction

  -- ** Binary Reduction
  , binaryReduction
  , binaryReductions

  -- *** Generic Binary Reduction
  , genericBinaryReduction

  -- *** Set Binary Reduction
  , ISetReducer
  , setBinaryReduction
  , toSetReducer

  -- ** Utils
  , binarySearch
  , liftISetReducer
  ) where

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
type Predicate m e =
  e -> m Bool

-- | A 'Reducer' is now a function that takes a list of elements and returns
-- the smallest list of elements such that the predicate is still satisfied.
-- The reducer will return nothing if no subset satisfies the predicate.
type Reducer m s =
  Predicate m s -> s -> m (Maybe s)

-- | An predicate is like a 'Predicate', but the success of the predicate is
-- now encoded in the monad itself.
type MPredicate m e =
  e -> m ()

-- | Like a 'Reducer', but uses an 'IS.IntSet' instead for performance gain.
type IReducer m =
  MPredicate (MaybeT m) IS.IntSet -> IS.IntSet -> MaybeT m IS.IntSet

-- ** Delta Debugging

-- | An implmentation of ddmin.
ddmin :: Monad m => Reducer m [e]
ddmin p es = do
  t' <- p []
  if t'
    then return $ Just []
    else do
      mx <- unsafeDdmin p es
      case mx of
        Just x -> do
          t <- p x
          return $ if t then Just x else Nothing
        Nothing -> return Nothing

-- | A slightly faster ddmin, but does not check the predicate for the empty set first and
-- the result of the ddmin. So this assumes that P [] = false and P U = true.
unsafeDdmin :: Monad m => Reducer m [e]
unsafeDdmin =
  liftReducer (ddmin' 2)

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

-- | Linary reduction is just going through the set, event by event and
-- remove an element at a time.
--
-- Runtime: \( O(n) \)
linearReduction :: Monad m => Reducer m [e]
linearReduction p xs =
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
--
-- Runtime: \(O(s \log n)\)
binaryReduction :: Monad m => Reducer m [e]
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

-- | Find all possible minimas. Worst-case exponential.
binaryReductions :: (Eq e, Monad m) => Predicate m [e] -> [e] -> m [[e]]
binaryReductions p =
  fmap (map L.reverse) . go []
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


-- $GenericReducers
-- A set reducer is able to take a list of sets, and then produce a
-- smaller combined set such that a predicate is still unhold.
--
-- Set reducers only makes sense when the sets are overlapping or if there are
-- multiple minima. In that case there is a big difference between
-- returning a single set of size N or 2 sets of size 1. We want to strive to
-- return as small final set as possible.
--

-- | Like a the binary reductor, but uses a generic cost function. Is functionally
-- equivilent to 'binaryReduction' if the const function is 'List.length'.
genericBinaryReduction :: (Monad m, Show a) => ([a] -> Int) -> Reducer m [a]
genericBinaryReduction cost (liftPredicate -> pred) =
  runMaybeT . go []
  where
    go !sol (L.sortOn (cost . (:sol)) -> !as) = do
      r <- binarySearch (pred . (\i -> L.take i as ++ sol)) 0 (L.length as)
      if r > 0
        then do
          let (as', rs:ys) = L.splitAt (r - 1) as
          go (rs:sol) as' <|> return (as' ++ rs:sol)
        else
          return sol

-- | An 'ISetReducer' like a generic reducer but uses slightly optimized
-- data-structures.
type ISetReducer m =
  Predicate m IS.IntSet -> [IS.IntSet] -> m (Maybe [IS.IntSet])

-- | SetBinaryReduction is much like regular binary reduction, but with
-- one added feature. Since we want the smallest output set we have to
-- continuously sort the list of set in size  to get the smallest
-- possible set.
setBinaryReduction :: Monad m => ISetReducer m
setBinaryReduction (liftPredicate -> pred) =
  runMaybeT . go ([], IS.empty) . map (\a -> (a, a))
  where
    go (sol, h) (L.sortOn (IS.size . snd) -> !as) = do
      let u = V.fromList $ L.scanl (\a -> IS.union a . snd) h as
      r <- binarySearch (pred . V.unsafeIndex u) 0 (V.length u - 1)
      if r > 0
        then do
          let
            (as', (rs, ru):_) = L.splitAt (r - 1) as
            h' = IS.union h ru
          cases
            [ pred h' >> return (rs:sol)
            , go (rs:sol, IS.union h ru)
                [(a, s') | (a, s) <- as', let s' = s IS.\\ ru, not (IS.null s')]
                <|> return (map fst as' ++ rs:sol)
            ]
        else
          return sol

-- * Utilities

-- | binarySearch, returns a number between lw and hg that satisfies
-- the predicate. It requires that if p is monotone.
-- $p n = true then p n-1 = true$. fails if no such number exists.
binarySearch :: (MonadPlus m) => MPredicate m Int -> Int -> Int -> m Int
binarySearch p !lw !hg = do
  let pivot = lw + ((hg - lw) `quot` 2)
  cases
    [ p pivot
      >> if lw == pivot
          then return lw
          else binarySearch p lw (pivot -1) <|> return pivot
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

-- | Transform a Predicate to a MPredicate
liftPredicate :: Monad m => Predicate m e -> MPredicate (MaybeT m) e
liftPredicate pred =
  lift . pred >=> guard

-- | Transform a IReducer to a Reducer
liftReducer :: Monad m => IReducer m -> Reducer m [e]
liftReducer red pred es = do
  mr <- runMaybeT $ red (liftPredicate pred . unset) world
  return $ unset <$> mr
  where
    refs = V.fromList es
    world = IS.fromAscList [0..(V.length refs - 1)]
    unset = map (V.unsafeIndex refs) . IS.toAscList

-- | Transform a ISetReducer to a Reducer
toSetReducer :: Monad m => Reducer m [IS.IntSet] -> ISetReducer m
toSetReducer red pred es =
  red (pred . IS.unions) $ L.sortOn IS.size es

-- | Transform a ISetReducer to a Reducer
liftISetReducer :: Monad m => ISetReducer m -> Reducer m [a]
liftISetReducer red pred es = do
  mr <- red (pred . unset) world
  return $ unset . IS.unions <$> mr
  where
    refs = V.fromList es
    world = [IS.singleton i | i <- [0..(V.length refs - 1)]]
    unset = map (V.unsafeIndex refs) . IS.toAscList
