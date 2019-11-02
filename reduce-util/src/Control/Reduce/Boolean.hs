{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Control.Reduce.Boolean where

import Prelude hiding (not, and, or)
import qualified Prelude
import Data.Monoid

-- lens
import Control.Lens hiding (andOf, orOf)

class Boolean a where
  infixl 4 \/
  infixl 5 /\
  infixr 3 ==>
  (/\), (\/), (==>) :: a -> a -> a
  true :: a
  false :: a
  not :: a -> a

  a ==> b = not a \/ b

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


data Term a
  = TAnd (Term a) (Term a)
  | TOr  (Term a) (Term a)
  | TNot (Term a)
  | TVar !a
  | TConst !Bool

instance Boolean (Term a) where
  (/\)  = TAnd
  (\/)  = TOr
  not a = TNot a
  true  = TConst True
  false = TConst False
  {-# INLINE (/\) #-}
  {-# INLINE (\/) #-}
  {-# INLINE not #-}

type Cnf a = [[(Bool, a)]]

cnfCompiler :: Term a -> Cnf a
cnfCompiler = undefined -- \case
  -- TAnd clauses ->
  --   concatMap cnfCompiler clauses
  -- TOr terms ->
  --   concat $ mapM cnfCompiler (toList terms)
  -- TNot b -> map (map (over _1 not)) $ cnfCompiler b
