{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE ViewPatterns  #-}
{-|
Module      : Control.Reduce
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module defines a set of reducers. A reducer is a function that
given a predicate reduces a set of items to a smaller set of items.

-}
module Control.Reduce
  ( PredicateM (..)
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
  , unsafeLinearReduction

  -- ** Binary Reduction
  , binaryReduction

  -- *** Generic Binary Reduction
  , genericBinaryReduction

  -- *** Set Binary Reduction
  , GuardM (..)
  , ISetReducer
  , setBinaryReduction
  , toSetReducer

  -- ** Utils
  , binarySearch
  , liftISetReducer
  ) where

-- mtl
import           Control.Monad.Trans.Maybe

-- contravariant
import           Data.Functor.Contravariant            hiding (Predicate)

-- base
import           Control.Applicative
import           Control.Monad
import           Data.Functor
import qualified Data.IntSet                           as IS
import qualified Data.List                             as L
import qualified Data.Vector                           as V

-- reduce
import           Data.Functor.Contravariant.PredicateM


-- * Reducers

-- | A 'Reducer' is now a function that takes a list of elements and returns
-- the smallest list of elements such that the predicate is still satisfied.
-- The reducer will return nothing if no subset satisfies the predicate.
type Reducer m s =
  PredicateM m s -> s -> m (Maybe s)

-- | Like a 'Reducer', but uses an 'IS.IntSet' instead for performance gain.
type IReducer m =
  GuardM (MaybeT m) IS.IntSet -> IS.IntSet -> MaybeT m IS.IntSet

-- ** Delta Debugging

-- | An implmentation of ddmin.
ddmin :: Monad m => Reducer m [e]
ddmin p es = do
  t' <- runPredicateM p []
  if t'
    then return $ Just []
    else do
      mx <- unsafeDdmin p es
      case mx of
        Just x -> do
          t <- runPredicateM p x
          return $ if t then Just x else Nothing
        Nothing -> return Nothing

-- | A slightly faster ddmin, but does not check the predicate for the empty set
-- first and the result of the ddmin. So this assumes that P [] = false and P U
-- = true.
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
    testrec n' d = runGuardM test d >> ddmin' n' test d
    deltas = splitSet n world
    size = IS.size world

-- | Linary reduction is just going through the set, event by event and
-- remove an element at a time.
--
-- Runtime: \( O(n) \)
linearReduction :: Monad m => Reducer m [e]
linearReduction (asMaybeGuard -> p) xs = do
  runMaybeT $ do
    runGuardM p xs
    unsafeLinearReduction p xs

unsafeLinearReduction :: MonadPlus m => GuardM m [e] -> [e] -> m [e]
unsafeLinearReduction p = go []
  where
    go !sol = \case
      [] -> return sol
      e:es' -> cases
        [ runGuardM p (sol ++ es') >> go sol es'
        , go (sol ++ [e]) es'
        ]

-- | Binary reduction is the simplest form of the set minimizing algorithm.
-- As such it is fast, while still having the 1-minimality property.
--
-- Runtime: \(O(s \log n)\)
binaryReduction :: Monad m => Reducer m [e]
binaryReduction (asMaybeGuard -> p) es =
  runMaybeT . go [] $ L.length es
  where
    go !sol !n = cases
      [ takeIfSolution p sol
      , do
          r <- binarySearch (contramap range p) 0 n
          go (es L.!! (r - 1) : sol) (r - 1)
      ]
      where range i = L.take i es ++ sol


-- $GenericReducers
-- A set reducer is able to take a list of sets, and then produce a
-- smaller combined set such that a predicate is still unhold.
--
-- Set reducers only makes sense when the sets are overlapping or if there are
-- multiple minima. In that case there is a big difference between
-- returning a single set of size N or 2 sets of size 1. We want to strive to
-- return as small final set as possible.

-- | Like a the binary reductor, but uses a generic cost function. Is functionally
-- equivilent to 'binaryReduction' if the const function is 'List.length'.
genericBinaryReduction :: (Monad m) => ([a] -> Int) -> Reducer m [a]
genericBinaryReduction cost (asMaybeGuard -> p) =
  runMaybeT . go []
  where
    go !sol (L.sortOn (cost . (:sol)) -> !as) = cases
      [ takeIfSolution p sol
      , do
          r <- binarySearch (contramap range p) 0 (L.length as)
          let (as', rs:_) = L.splitAt (r - 1) as
          go (rs:sol) as' <|> return (as' ++ rs:sol)
      ]
      where range i = L.take i as ++ sol

-- | An 'ISetReducer' like a generic reducer but uses slightly optimized
-- data-structures.
type ISetReducer m =
  PredicateM m IS.IntSet -> [IS.IntSet] -> m (Maybe [IS.IntSet])

-- | SetBinaryReduction is much like regular binary reduction, but with
-- one added feature. Since we want the smallest output set we have to
-- continuously sort the list of set in size  to get the smallest
-- possible set.
setBinaryReduction :: Monad m => ISetReducer m
setBinaryReduction (asMaybeGuard -> pred') =
  runMaybeT . go ([], IS.empty) . map (\a -> (a, a))
  where
    go (sol, h) (L.sortOn (IS.size . snd) -> !as) = do
      let u = V.fromList $ L.scanl (\a -> IS.union a . snd) h as
      r <- binarySearch (contramap (V.unsafeIndex u) pred') 0 (V.length u - 1)
      if r > 0
        then do
          let
            (as', (rs, ru):_) = L.splitAt (r - 1) as
            h' = IS.union h ru
          cases
            [ runGuardM pred' h' >> return (rs:sol)
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
binarySearch :: (MonadPlus m) => GuardM m Int -> Int -> Int -> m Int
binarySearch p !lw !hg = do
  let pivot = lw + ((hg - lw) `quot` 2)
  cases
    [ runGuardM p pivot
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

    partition n' = \case
      [] -> []
      ss -> h : partition n' r
        where (h, r) = splitAt n' ss

-- ** MonadPlus related

-- | Given a list of cases, return the result of the first succeding
-- computation.
cases :: MonadPlus m => [m a] -> m a
cases = msum

-- | Take the solution if it exist.
takeIfSolution :: Monad m => GuardM m a -> a -> m a
takeIfSolution p a = runGuardM p a $> a

-- ** Conversion
-- | Transform a IReducer to a Reducer
liftReducer :: Monad m => IReducer m -> Reducer m [e]
liftReducer red pred' es = do
  mr <- runMaybeT $ red (contramap unset (asMaybeGuard pred')) world
  return $ unset <$> mr
  where
    refs = V.fromList es
    world = IS.fromAscList [0..(V.length refs - 1)]
    unset = map (V.unsafeIndex refs) . IS.toAscList

-- | Transform a ISetReducer to a Reducer
toSetReducer :: Monad m => Reducer m [IS.IntSet] -> ISetReducer m
toSetReducer red pred' es =
  red (contramap IS.unions pred') $ L.sortOn IS.size es

-- | Transform a ISetReducer to a Reducer
liftISetReducer :: Monad m => ISetReducer m -> Reducer m [a]
liftISetReducer red pred' es = do
  mr <- red (contramap unset pred') world
  return $ unset . IS.unions <$> mr
  where
    refs = V.fromList es
    world = [IS.singleton i | i <- [0..(V.length refs - 1)]]
    unset = map (V.unsafeIndex refs) . IS.toAscList
