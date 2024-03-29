{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Reduce.Boolean.LiteralSet where

-- lens
import Control.Lens

-- base
import Data.Semigroup
import Data.Coerce
import Data.Maybe
import Text.Show
import Prelude hiding (null, map)
import Data.Foldable hiding (null, toList)
import qualified Data.List as L

-- containers
import qualified Data.IntSet as IS

-- reduce-util
import Control.Reduce.Boolean

-- A integer literal
newtype IntLiteral = IntLiteral { internalInt :: Int }
  deriving (Show, Eq)

instance BooleanAlgebra IntLiteral where
  type LitVar IntLiteral = Int
  tt a = IntLiteral $ a
  ff a = IntLiteral $ a + minBound

toLiteral :: IntLiteral -> Literal Int
toLiteral (IntLiteral i)
  | i < 0 = Literal False (i - minBound)
  | otherwise = Literal True i
{-# INLINE toLiteral #-}

fromLiteral :: Literal Int -> IntLiteral
fromLiteral (Literal b i)
  | b = IntLiteral i
  | otherwise = IntLiteral (i + minBound)
{-# INLINE fromLiteral #-}

negateLiteral :: IntLiteral -> IntLiteral
negateLiteral (IntLiteral i)
  | i < 0     = IntLiteral $ i - minBound
  | otherwise = IntLiteral $ i + minBound

-- A integer literal set
newtype IntLiteralSet = IntLiteralSet { internalIntSet :: IS.IntSet }
  deriving (Eq, Ord, Semigroup, Monoid)

litEmpty :: IntLiteralSet
litEmpty = mempty

litMember :: IntLiteral -> IntLiteralSet -> Bool
litMember (IntLiteral l) (IntLiteralSet is) = l `IS.member` is

litInsert :: IntLiteral -> IntLiteralSet -> Maybe IntLiteralSet
litInsert l is
  | l' `member` is = Nothing
  | otherwise      = Just . IntLiteralSet $ internalInt l `IS.insert` internalIntSet is
  where l' = negateLiteral l

litUnion :: IntLiteralSet -> IntLiteralSet -> Maybe IntLiteralSet
litUnion is1 is2 =
  foldrM litInsert is1 (litToList is2)

litDifference :: IntLiteralSet -> IntLiteralSet -> IntLiteralSet
litDifference (IntLiteralSet is1) (IntLiteralSet is2) =
  IntLiteralSet (is1 `IS.difference` is2)

litSize :: IntLiteralSet -> Int
litSize (IntLiteralSet is1) =
  IS.size is1

litIsSubsetOf :: IntLiteralSet -> IntLiteralSet -> Bool
litIsSubsetOf (IntLiteralSet is1) (IntLiteralSet is2) =
  is1 `IS.isSubsetOf` is2
{-# inline litIsSubsetOf #-}

litMap :: (IntLiteral -> IntLiteral) -> IntLiteralSet -> Maybe IntLiteralSet
litMap fn = litFromList . L.map fn . litToList

litVmap :: (Int -> Int) -> IntLiteralSet -> Maybe IntLiteralSet
litVmap fn = litFromList . L.map (fromLiteral . fmap fn . toLiteral) . litToList

litFromList :: [IntLiteral] -> Maybe IntLiteralSet
litFromList = foldrM litInsert empty

litToList :: IntLiteralSet -> [IntLiteral]
litToList = coerce . IS.toList . internalIntSet

instance Show IntLiteralSet where
  showsPrec n c = showParen (n > 9) $
    showString "fromList " . (showListWith shows . L.map toLiteral . toList $ c)

class IsIntLiteralSet a where
  toLiteralSet   :: a -> IntLiteralSet
  fromLiteralSet :: IntLiteralSet -> a

instance IsIntLiteralSet IntLiteralSet where
  toLiteralSet   = id
  {-# INLINE toLiteralSet #-}
  fromLiteralSet = id
  {-# INLINE fromLiteralSet #-}

fromList :: IsIntLiteralSet a => [IntLiteral] -> Maybe a
fromList = fmap fromLiteralSet . litFromList
{-# INLINE fromList #-}

fromList' :: IsIntLiteralSet a => [IntLiteral] -> a
fromList' = fromJust . fromList
{-# INLINE fromList' #-}

toList :: IsIntLiteralSet a => a -> [IntLiteral]
toList = litToList . toLiteralSet
{-# INLINE toList #-}

empty :: IsIntLiteralSet a => a
empty = fromLiteralSet $ litEmpty


map :: IsIntLiteralSet a => (IntLiteral -> IntLiteral) -> a -> Maybe a
map fn = fmap fromLiteralSet . litMap fn . toLiteralSet
{-# INLINE map #-}

-- | Variable map
vmap :: IsIntLiteralSet a => (Int -> Int) -> a -> Maybe a
vmap fn = fmap fromLiteralSet . litVmap fn . toLiteralSet
{-# INLINE vmap #-}

member :: IsIntLiteralSet a => IntLiteral -> a -> Bool
member lit (toLiteralSet -> is) = lit `litMember` is
{-# INLINE member #-}

notMember :: IsIntLiteralSet a => IntLiteral -> a -> Bool
notMember li is = Prelude.not (li `member` is)
{-# INLINE notMember #-}

insert :: IsIntLiteralSet a => IntLiteral -> a -> Maybe a
insert lit (toLiteralSet -> is) = fromLiteralSet <$> lit `litInsert` is
{-# INLINE insert #-}

minView :: IsIntLiteralSet a => a -> Maybe (IntLiteral, a)
minView =
  fmap (\(a, b) -> (IntLiteral a, fromLiteralSet (IntLiteralSet  b)))
    . IS.minView . internalIntSet . toLiteralSet
{-# INLINE minView #-}

null :: IsIntLiteralSet a => a -> Bool
null (toLiteralSet -> IntLiteralSet is) =
  IS.null is
{-# INLINE null #-}

union :: IsIntLiteralSet a => a -> a -> Maybe a
union (toLiteralSet -> is1) (toLiteralSet -> is2) =
  fromLiteralSet <$> is1 `litUnion` is2
{-# INLINE union #-}

difference :: IsIntLiteralSet a => a -> a -> a
difference (toLiteralSet -> is1) (toLiteralSet -> is2) =
  fromLiteralSet (is1 `litDifference` is2)
{-# INLINE difference #-}

size :: IsIntLiteralSet a => a -> Int
size (toLiteralSet -> is1) =
  litSize is1
{-# INLINE size #-}

isSubsetOf :: IsIntLiteralSet a => a -> a -> Bool
isSubsetOf (toLiteralSet -> is1) (toLiteralSet -> is2) =
  is1 `litIsSubsetOf` is2
{-# INLINE isSubsetOf #-}

singleton :: IsIntLiteralSet a => IntLiteral -> a
singleton (IntLiteral a) = fromLiteralSet (IntLiteralSet (IS.singleton a))
{-# INLINE singleton #-}

unSingleton :: IsIntLiteralSet a => a -> Maybe IntLiteral
unSingleton a = case minView a of
  Just (x, xs)
    | null xs -> Just x
  _ -> Nothing
{-# INLINE unSingleton #-}

transpose :: IsIntLiteralSet a => a -> a
transpose (splitLiterals -> (ff', tt')) = fromJust $ joinLiterals (tt', ff')
{-# INLINE transpose #-}

toNegativeLiterals :: IS.IntSet -> IS.IntSet
toNegativeLiterals = IS.fromDistinctAscList . L.map (\a -> a + minBound) . IS.toAscList

fromNegativeLiterals :: IS.IntSet -> IS.IntSet
fromNegativeLiterals = IS.fromDistinctAscList . L.map (\a -> a - minBound) . IS.toAscList

splitLiterals :: IsIntLiteralSet a => a -> (IS.IntSet, IS.IntSet)
splitLiterals (toLiteralSet -> IntLiteralSet is) =
  (fromNegativeLiterals falses, trues)
  where (falses, trues) = IS.split (negate 1) is
{-# INLINE splitLiterals #-}

joinLiterals :: IsIntLiteralSet a => (IS.IntSet, IS.IntSet) -> Maybe a
joinLiterals (falses, trues)
  | falses `IS.disjoint` trues = Just (joinLiterals' (falses, trues))
  | otherwise = Nothing
{-# INLINE joinLiterals #-}

-- | Join the literals but do not check that the two sets are disjoint
joinLiterals' :: IsIntLiteralSet a => (IS.IntSet, IS.IntSet) -> a
joinLiterals' (falses, trues) =
  fromLiteralSet . IntLiteralSet $ toNegativeLiterals falses `IS.union` trues
{-# INLINE joinLiterals' #-}

variables :: IsIntLiteralSet a => a -> IS.IntSet
variables = view both . splitLiterals
{-# INLINE variables #-}


-- | A Clause is a disjunction of literals. This can be
-- represented as an `IS.IntSet` of variable ids. The
-- negative literals are the variable ids added with the minimum bound.
newtype Clause = Clause { clauseSet :: IntLiteralSet }
  deriving Eq  via IntLiteralSet
  deriving Ord via IntLiteralSet
  deriving Show via IntLiteralSet

instance IsIntLiteralSet Clause where
  toLiteralSet   = clauseSet
  {-# inline toLiteralSet #-}
  fromLiteralSet = Clause
  {-# inline fromLiteralSet #-}

emptyClause :: Clause
emptyClause = Clause mempty

conditionClause :: IntLiteral -> Clause -> Maybe Clause
conditionClause (IntLiteral x) (Clause (IntLiteralSet s)) =
  if x < 0
  then
    if x `IS.member` s
    then Nothing
    else Just . Clause . IntLiteralSet $ (x + minBound) `IS.delete` s
  else
    if x `IS.member` s
    then Nothing
    else Just . Clause . IntLiteralSet $ (x - minBound) `IS.delete` s

-- | Given a injective mapping of variables to potential new variables,
-- remap the clause, and set everything to false that does not
-- exist. Returns Nothing if the clause is satisified.
limitClause :: (Int -> Maybe Int) -> Clause -> Maybe Clause
limitClause fn (Clause c) =
  mapM fn (IS.toList falses) <&> \falses' ->
    fromMaybe (error "Mapping not injective") $ joinLiterals
      ( IS.fromList falses'
      , IS.fromList $ mapMaybe fn (IS.toList trues)
      )
 where (falses, trues) = splitLiterals c

-- | Build a positive clause from a intset
-- Does not check that the set is positive integers.
mkPositiveClause :: IS.IntSet -> Clause
mkPositiveClause is = Clause (IntLiteralSet is)

hasPositiveClause :: Clause -> Bool
hasPositiveClause clause = Prelude.not (IS.null trues)
 where (_, trues) = splitLiterals clause

hasNegativeClause :: Clause -> Bool
hasNegativeClause clause = Prelude.not (IS.null falses)
 where (falses, _) = splitLiterals clause

displayImplication :: (Int -> ShowS) -> Clause -> ShowS
displayImplication showKey c =
  if null c
  then showString "false"
  else showsFalses . showString " ==> " . showsTrues
 where
  showsFalses
    | IS.null falses = showString "true"
    | otherwise = showItems " /\\ " falses

  showsTrues
    | IS.null trues = showString "false"
    | otherwise = showItems " \\/ " trues

  (falses, trues) = splitLiterals c
  showItems del =
    appEndo . foldMap Endo . L.intersperse (showString del) . L.map showKey . IS.toList


-- | A Term is a conjuction of literals. This can be
-- represented as an `IS.IntSet` of variable ids. The
-- negative literals are the variable ids added with the minimum bound.
newtype Term = Term { termSet :: IntLiteralSet }
  deriving Eq  via IntLiteralSet
  deriving Ord via IntLiteralSet
  deriving Show via IntLiteralSet

instance IsIntLiteralSet Term where
  toLiteralSet   = termSet
  {-# inline toLiteralSet #-}
  fromLiteralSet = Term
  {-# inline fromLiteralSet #-}

emptyTerm :: Term
emptyTerm = Term mempty

termAddLiteral :: Literal Int -> Term -> Maybe Term
termAddLiteral (Literal b v) (Term (IntLiteralSet is))
  | b     =
    if v `IS.member` is
    then Nothing
    else Just $ Term (IntLiteralSet (v `IS.insert` is))
  | otherwise =
    if v' `IS.member` is
    then Nothing
    else Just $ Term (IntLiteralSet (v' `IS.insert` is))
  where
    v' = v + minBound

termFromLiterals :: Foldable t => t (Literal Int) -> Maybe Term
termFromLiterals = foldrM termAddLiteral emptyTerm
