{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
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
import Data.Coerce
-- import Data.Foldable (toList)

-- import qualified Data.List as List
-- import Data.Bits
-- import Data.Int

-- import Data.Bifoldable
-- import Data.Bitraversable

-- containers
-- import qualified Data.IntSet as IS

-- vector
--import qualified Data.Vector.Unboxed as UV


-- lens
import Control.Lens hiding (andOf, orOf)

-- | A boolean logic
class Boolean a where
  infixr 2 \/
  infixr 3 /\
  infixr 1 ==>
  (/\), (\/), (==>) :: a -> a -> a
  true :: a
  false :: a
  not :: a -> a
  a ==> b = not a \/ b

(∧) :: Boolean a => a -> a -> a
(∧) = (/\)
(∨) :: Boolean a => a -> a -> a
(∨) = (\/)
infixl 3 ∧
infixl 2 ∨

-- | A synonym for 'not'
neg :: Boolean a => a -> a
neg = not

instance Boolean Bool where
  (/\) = (&&)
  (\/) = (||)
  not = Prelude.not
  true = True
  false = False

class BooleanAlgebra a where
  type BooleanVar a
  tt :: BooleanVar a -> a
  ff :: BooleanVar a -> a


-- newtype And a = And { runAnd :: a }
--   deriving (Show, Boolean)
-- newtype Or a = Or { runOr :: a }
--   deriving (Show, Boolean)

-- instance Boolean a => Semigroup (And a) where (<>) = (/\)
-- instance Boolean a => Monoid (And a) where mempty = true

-- instance Boolean a => Semigroup (Or a) where (<>) = (\/)
-- instance Boolean a => Monoid (Or a) where mempty = false

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

data Fix f = Fix { unFix :: f (Fix f) }

deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Ord (f (Fix f)) => Ord (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)

class Functor f => Fixed f b | b -> f where
  cata :: (f a -> a) -> b -> a
  default cata :: Coercible (Fix f) b => (f a -> a) -> b -> a
  cata f = f . fmap (cata f) . unFix . coerce

  ana  :: (a -> f a) -> a -> b
  default ana :: Coercible b (Fix f) => (a -> f a) -> a -> b
  ana f = coerce . Fix . fmap (ana f) . f

liftF :: (Coercible b (Fix f), Coercible (Fix f) b, Functor f) => f b -> b
liftF = coerce . Fix . fmap coerce

unliftF :: (Coercible b (Fix f), Coercible (Fix f) b, Functor f) => b -> f b
unliftF = fmap coerce . unFix . coerce

hylo :: forall f y x. (Fixed f x, Fixed f y) => (f y -> y) -> (x -> f x) -> x -> y
hylo phi psi x = cata phi (ana psi x :: y)

-- -- | A function that unpacks and packs itself into its self.
-- isohylo :: forall f x. Fixed f x => (f x -> f x) -> x -> x
-- isohylo phi x = cata phi -- (ana psi x :: y)

instance Functor f => Fixed f (Fix f) where

-- | Term is a free boolean structure with variables.
data TermF a f
  = TAnd f f
  | TOr  f f
  | TNot f
  | TVar !a
  | TConst !Bool
  deriving
    ( Eq, Ord
    , Functor, Foldable, Traversable
    )

instance Bifunctor TermF where
  bimap f s = \case
    TAnd a b -> TAnd (s a) (s b)
    TOr  a b -> TOr (s a) (s b)
    TNot a   -> TNot (s a)
    TVar a   -> TVar (f a)
    TConst b -> TConst b

-- instance Bifoldable TermF where
-- instance Bitraversable TermF where
 
newtype Term a = Term (Fix (TermF a))
  deriving (Eq, Ord)

instance Functor Term where
  fmap f = cata $ liftF . \case
    TAnd a b -> TAnd a b
    TOr a b -> TOr a b
    TNot a -> TNot a
    TVar l -> TVar (f l)
    TConst b -> TConst b

instance Foldable Term where
  foldMap f = cata \case
    TAnd a b -> a <> b
    TOr a b -> a <> b
    TNot a -> a
    TVar l -> f l
    TConst _ -> mempty

instance Traversable Term where
  traverse f = cata $ fmap liftF . \case
    TAnd a b -> TAnd <$> a <*> b
    TOr a b -> TOr <$> a <*> b
    TNot a -> TNot <$> a
    TVar l -> TVar <$> f l
    TConst b -> pure $ TConst b

instance Fixed (TermF a) (Term a)

instance Boolean (Term a) where
  (/\) a b = liftF (TAnd a b)
  (\/) a b = liftF (TOr a b)
  not   = liftF . TNot
  true  = liftF (TConst True)
  false = liftF (TConst False)
  {-# INLINE (/\) #-}
  {-# INLINE (\/) #-}
  {-# INLINE not #-}
  {-# INLINE false #-}
  {-# INLINE true #-}

instance BooleanAlgebra (Term a) where
  type BooleanVar (Term a) = a
  tt a = liftF (TVar a)
  ff a = neg (liftF (TVar a))

showsPrecTermF :: Show a => TermF a (Int -> ShowS) -> Int -> ShowS
showsPrecTermF = \case
  TAnd a b -> \n -> showParen (n > 3) (a 3 . showString " ∧ " . b 2)
  TOr a b  -> \n -> showParen (n > 2) (a 2 . showString " ∨ " . b 1)
  TNot a -> \n -> showParen (n > 9) (showString "not " . a 10)
  TVar i -> \n -> showParen (n > 9) (showString "tt " . showsPrec 10 i)
  TConst True -> const $ showString "true"
  TConst False -> const $ showString "false"

instance Show a => Show (Term a) where
  showsPrec n f = cata showsPrecTermF f n

toTerm :: Fixed (TermF a) x => x -> Term a
toTerm = cata liftF
{-# INLINE toTerm #-}

fromTerm :: Fixed (TermF a) x => Term a -> x
fromTerm = ana unliftF
{-# INLINE fromTerm #-}

crossCompiler :: (Fixed (TermF a) x, Fixed (TermF a) y) => x -> y
crossCompiler = fromTerm . toTerm


-- | A literal is either true or false.
data Literal = Literal {-# UNPACK #-} !Bool {-# UNPACK#-} !Int
  deriving (Eq, Ord)

instance BooleanAlgebra Literal where
  type BooleanVar Literal = Int
  tt = Literal True
  ff = Literal False

instance Show Literal where
  showsPrec n (Literal b i) =
    showParen (n > 9) $
    showString (if b then "tt " else "ff ") . showsPrec 10 i

-- | Negation normal form
data NnfF f
  = NAnd f f
  | NOr f f
  | NLit {-# UNPACK #-} !Literal
  | NConst !Bool
  deriving (Eq, Ord, Functor, Foldable, Traversable)

newtype Nnf = Nnf (Fix NnfF)
  deriving (Eq, Ord)

instance Fixed NnfF Nnf

newtype NnfAsTerm = NnfAsTerm { nnfAsTerm :: Nnf }
  deriving (Eq, Ord)

instance Fixed (TermF Int) NnfAsTerm where
  -- cata :: (TermF Int a -> a) -> b -> a
  cata fn = cata (fn . handle) . nnfAsTerm where
    handle = \case
      NAnd a b -> TAnd a b
      NOr a b -> TOr a b
      NLit (Literal True i) -> TVar i
      NLit (Literal False i) -> TNot (fn $ TVar i)
      NConst b -> TConst b

  -- ana  :: (a -> TermF Int a) -> a -> b
  ana fn = NnfAsTerm . ana (uncurry go) . (True,) . fn where
    go n = \case
      TAnd t1 t2 ->
        (if n then NAnd else NOr) (n, fn t1) (n, fn t2)
      TOr  t1 t2 ->
        (if n then NOr else NAnd) (n, fn t1) (n, fn t2)
      TNot t1 ->
        go (not n) (fn t1)
      TVar a ->
        NLit (Literal n a)
      TConst b ->
        NConst (if n then b else not b)

instance Show Nnf where
  showsPrec n f = cata showsPrecTermF (NnfAsTerm f) n

instance Show NnfAsTerm where
  showsPrec n f = cata showsPrecTermF f n

instance Boolean Nnf where
  a /\ b  = liftF $ NAnd a b
  a \/ b  = liftF $ NOr a b
  not     = nnfAsTerm . fromTerm . neg . toTerm . NnfAsTerm
  true    = liftF $ NConst True
  false   = liftF $ NConst False
  {-# INLINE (/\) #-}
  {-# INLINE (\/) #-}
  {-# INLINE not #-}

instance BooleanAlgebra Nnf where
  type BooleanVar Nnf = Int
  tt = liftF . NLit . tt
  ff = liftF . NLit . ff

-- | Flattens an nnf
flattenNnf :: Nnf -> Nnf
flattenNnf = liftF . cata handle where
  handle = \case
    NAnd (NConst True) b  -> b
    NAnd b (NConst True)  -> b
    NAnd (NConst False) _ -> NConst False
    NAnd _ (NConst False) -> NConst False
    NAnd a b              -> NAnd (liftF a) (liftF b)

    NOr (NConst True) _   -> NConst True
    NOr _ (NConst True)   -> NConst True
    NOr (NConst False) t  -> t
    NOr t (NConst False)  -> t
    NOr a b               -> NOr (liftF a) (liftF b)

    NConst t              -> NConst t
    NLit l                -> NLit l

-- | The dependency language
data Dependency
  = DFalse
  | DLit !Literal
  | DDeps {-# UNPACK#-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord)

infixr 5 ~~>
(~~>) :: Int -> Int -> Dependency
(~~>) = DDeps

instance BooleanAlgebra Dependency where
  type BooleanVar Dependency = Int
  tt = DLit . tt
  ff = DLit . ff

instance Show Dependency where
  showsPrec n = \case
    DFalse    -> showString "DFalse"
    DLit    l -> showsPrec n l
    DDeps i j -> showParen (n > 4) (showsPrec 5 i . showString " ~~> " . showsPrec 5 j)

overNnfF :: NnfF (Dependency -> [Dependency]) -> Dependency -> [Dependency]
overNnfF = \case
  NAnd a b -> \d -> a d ++ b d
  NOr a b -> \ d -> a d >>= \case
    x@(DDeps _ _) -> [x]
    x -> b x
  NLit l@(Literal n a) -> \case
    DFalse -> [ DLit l ]
    DLit (Literal t b)
      | n /\ not t -> [ DDeps b a ]
      | not n /\ t -> [ DDeps a b ]
    d -> [ d ] -- overapproximate
  NConst b -> \d -> [ d | not b ]

underNnfF :: NnfF (Dependency -> [Dependency]) -> Dependency -> [Dependency]
underNnfF = \case
  NAnd a b -> \d -> a d ++ b d
  NOr a b -> \d -> [ y | x <- a d , y <- b x]
  NLit l@(Literal n a) -> \case
    DFalse -> [ DLit l ]
    DLit (Literal t b)
      | n /\ not t -> [ DDeps b a ]
      | not n /\ t -> [ DDeps a b ]
    d@(DDeps f t)
      | a == if n then t else f -> [ d ]
    _ -> [ ] -- underapproximate
  NConst b -> \d -> [ d | not b ]

underDependencies :: Nnf -> [Dependency]
underDependencies a =
  cata underNnfF a DFalse

overDependencies :: Nnf -> [Dependency]
overDependencies a =
  cata overNnfF a DFalse
