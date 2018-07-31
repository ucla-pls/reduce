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

import           Data.Functor
import           Data.Monoid

import qualified Data.IntSet as IS
import qualified Data.Vector as V
import qualified Data.List as L

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
    , cases [ testrec (max (n - 1) 2) (world IS.\\ d) | d <- deltas ]
    , guard (n < size) >> ddmin' (min size (2*n)) test world
    , return world
    ]
  where
    testrec n d = test d >> ddmin' n test d
    deltas = splitSet n world
    size = IS.size world

-- ** Binary Reduction

-- | Binary reduction is the simplest form of the set minimizing algorithm.
-- As such it is fast, while still having the 1-minimality property.
binaryReduction :: (Show e, Monad m) => Reducer [e] m
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

-- | Reversing Binary reduction is like the binary reduction, except it
-- reveres the order of the list for each iteration.
revBinaryReduction :: forall e m. (Show e, Monad m) => Reducer [e] m
revBinaryReduction p es = do
  runMaybeT $ go [] [0..(V.length ref - 1)]
  where
    ref = V.fromList es
    pred = liftPredicate p
    unwrap :: [Int] -> [e]
    unwrap = map (V.unsafeIndex ref) . L.sort
    go !sol sm = do
      r <- binarySearch (pred . range) 0 (L.length sm)
      cases
        [ do
            guard (r > 0)
            let sol' = (sm L.!! (r - 1) : sol)
            go sol' (L.reverse $ L.take (r - 1) sm)
        , return $ range r
        ]
      where
        range i = (unwrap $ L.take i sm ++ sol)

-- * Utilities

-- | binarySearch, returns a number between lw and hg that satisfies
-- the predicate. It requires that if p is monotone.
-- $p n = true then p n-1 = true$. fails if no such number exists.
binarySearch :: (MonadPlus m) => MPredicate Int m -> Int -> Int -> m Int
binarySearch p lw hg = do
  x <- go lw hg
  x `onlyifM` p
  where
    go !lw !hg = do
      let pivot = lw + ((hg - lw) `quot` 2)
      cases
        [ lw `onlyif` (== hg)
        , p pivot >> binarySearch p lw pivot
        , binarySearch p (pivot + 1) hg
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
