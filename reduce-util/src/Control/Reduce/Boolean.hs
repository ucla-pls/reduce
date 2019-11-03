{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Reduce.Boolean where

import Prelude hiding (not, and, or)
import qualified Prelude
import Data.Monoid
import Data.Foldable (toList)
import qualified Data.List as List
import Data.Bits
import Data.Int
import Control.Monad

-- containers
import qualified Data.IntSet as IS

-- vector
--import qualified Data.Vector.Unboxed as UV

-- lens
import Control.Lens hiding (andOf, orOf)

-- | A boolean logic
class Boolean a where
  infixl 4 \/
  infixl 5 /\
  infixr 3 ==>
  (/\), (\/), (==>) :: a -> a -> a
  true :: a
  false :: a
  not :: a -> a

  a ==> b = not a \/ b

-- | A synonym for 'not'
neg :: Boolean a => a -> a
neg = not

instance Boolean Bool where
  (/\) = (&&)
  (\/) = (||)
  not = Prelude.not
  true = True
  false = False

newtype And a = And { runAnd :: a }
  deriving (Show, Boolean)
newtype Or a = Or { runOr :: a }
  deriving (Show, Boolean)

instance Boolean a => Semigroup (And a) where (<>) = (/\)
instance Boolean a => Monoid (And a) where mempty = true

instance Boolean a => Semigroup (Or a) where (<>) = (\/)
instance Boolean a => Monoid (Or a) where mempty = false

and :: (Foldable f, Boolean a) => f a -> a
and = andOf folded
{-# INLINE and #-}

or :: (Foldable f, Boolean a) => f a -> a
or = orOf folded
{-# INLINE or #-}

andOf :: (Boolean b) => Getting (Endo (Endo b)) s b -> s -> b
andOf l = foldlOf' l (/\) true
{-# INLINE andOf #-}

orOf :: (Boolean b) => Getting (Endo (Endo b)) s b -> s -> b
orOf l = foldlOf' l (\/) false
{-# INLINE orOf #-}

forall :: (Boolean b, Foldable f) => f a -> (a -> b) -> b
forall = forallOf folded
{-# INLINE forall #-}

forallOf :: (Boolean b) => Getting (Endo (Endo b)) s a -> s -> (a -> b) -> b
forallOf l a fn = andOf (l.to fn) a
{-# INLINE forallOf #-}

exist :: (Boolean b, Foldable f) => f a -> (a -> b) -> b
exist = existOf folded
{-# INLINE exist #-}

existOf :: (Boolean b) => Getting (Endo (Endo b)) s a -> s -> (a -> b) -> b
existOf l a fn = orOf (l.to fn) a
{-# INLINE existOf #-}

given :: Boolean b => Bool -> b -> b
given b m = if b then m else true

-- | Term is a free boolean structure with variables.
data Term a
  = TAnd (Term a) (Term a)
  | TOr  (Term a) (Term a)
  | TNot (Term a)
  | TVar !a
  | TConst !Bool
  deriving (Show, Functor, Foldable)

instance Boolean (Term a) where
  (/\)  = TAnd
  (\/)  = TOr
  not a = TNot a
  true  = TConst True
  false = TConst False
  {-# INLINE (/\) #-}
  {-# INLINE (\/) #-}
  {-# INLINE not #-}

-- | We are currently operating at 31 bits.
type VarId = Int32

-- | Split a variable into it's components
splitVar :: VarId -> (Bool, Int32)
splitVar i = (not $ testBit i 31, clearBit i 31)
{-# INLINE splitVar #-}

-- | Split a variable into it's components
joinVar :: (Bool, Int32) -> VarId
joinVar (b, i) = if b then clearBit i 31 else setBit i 31
{-# INLINE joinVar #-}

notvar :: VarId -> VarId
notvar i = complementBit i 31
{-# INLINE notvar #-}

-- | A Clause is a unboxed sorted vector of 'Var's.
data Clause = Clause IS.IntSet IS.IntSet
  deriving (Eq, Ord)

instance Show Clause where
  showsPrec n (Clause tr fl) =
    showParen (n > 9) $
    showString "clause "
    . showList (
    map (True,) (IS.toList tr)
    ++ map (False,) (IS.toList fl)
    )

clause :: (Foldable f) => f (Bool, Int) -> Clause
clause (toList -> f) =
  Clause
  (IS.fromList . map snd $ trues)
  (IS.fromList . map snd $ falses)
  where
    (trues, falses) = List.partition fst f
{-# INLINE clause #-}

negateClause :: Clause -> [Clause]
negateClause (Clause tr fl) =
  [ Clause mempty (IS.singleton i) | i <- IS.toList tr]
  ++ [ Clause (IS.singleton i) mempty | i <- IS.toList fl]
{-# INLINE negateClause #-}

joinClause :: Clause -> Clause -> Clause
joinClause (Clause trs1 fls1) (Clause trs2 fls2) =
  Clause
  (IS.union trs1 trs2)
  (IS.union fls1 fls2)
{-# INLINE joinClause #-}

flattenClause :: Clause -> Maybe Clause
flattenClause a@(Clause tr fl) =
  if tr `IS.disjoint` fl
  then Just a
  else Nothing

-- | A CNF is a list of clauses
type Cnf = [Clause]

instance Boolean Cnf where
  a /\ b = a ++ b

  (\/) = liftM2 joinClause

  not = \case
    [] -> false
    c : rest ->
     negateClause c \/ not rest

  true  = []
  false = [clause []]

cnfCompiler :: Term Int -> Cnf
cnfCompiler = go where
  go = \case
    TAnd t1 t2   -> go t1 /\ go t2
    TOr t1 t2    -> go t1 \/ go t2
    TNot t1      -> not (go t1)
    TVar v       -> [ clause [(True, v)] ]
    TConst True  -> true
    TConst False -> false



-- go where
--   go = \case
--     TAnd t1 t2 -> go t1 ++ go t2
--     TOr
--   undefined -- \case
--   -- TAnd clauses ->
  --   concatMap cnfCompiler clauses
  -- TOr terms ->
  --   concat $ mapM cnfCompiler (toList terms)
  -- TNot b -> map (map (over _1 not)) $ cnfCompiler b
