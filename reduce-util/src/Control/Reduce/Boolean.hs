{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

-- deriving Functor Term where
--   fmap f =


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
data Nnf
  = NAnd Nnf Nnf
  | NOr Nnf Nnf
  | NLit {-# UNPACK #-} !Literal
  | NConst !Bool
  deriving (Eq)

instance Fixed (TermF Int) Nnf where
  -- cata :: (TermF Int a -> a) -> b -> a
  cata fn = \case
    NAnd a b ->
      fn $ TAnd (cata fn a) (cata fn b)
    NOr a b ->
      fn $ TOr  (cata fn a) (cata fn b)
    NLit (Literal True i) ->
      fn $ TVar i
    NLit (Literal False i) ->
      fn $ TNot (fn $ TVar i)
    NConst b ->
      fn $ TConst b

  -- ana  :: (a -> TermF Int a) -> a -> b
  ana fn = go True . fn where
    go n = \case
      TAnd t1 t2 ->
        (if n then NAnd else NOr) (go n $ fn t1) (go n $ fn t2)
      TOr  t1 t2 ->
        (if n then NOr else NAnd) (go n $ fn t1) (go n $ fn t2)
      TNot t1 ->
        go (not n) $ fn t1
      TVar a ->
        NLit (Literal n a)
      TConst b ->
        NConst (if n then b else not b)

instance Show Nnf where
  showsPrec n f = cata showsPrecTermF f n

instance Boolean Nnf where
  (/\)  = NAnd
  (\/)  = NOr
  not   = fromTerm . neg . toTerm
  true  = NConst True
  false = NConst False
  {-# INLINE (/\) #-}
  {-# INLINE (\/) #-}
  {-# INLINE not #-}

instance BooleanAlgebra Nnf where
  type BooleanVar Nnf = Int
  tt = NLit . tt
  ff = NLit . ff

-- | Flattens an nnf
flattenNnf :: Nnf -> Nnf
flattenNnf = \case
  NAnd a b ->
    case (flattenNnf a, flattenNnf b) of
      (NConst True, t)  -> t
      (t, NConst True)  -> t
      (NConst False, _) -> NConst False
      (_, NConst False) -> NConst False
      (a', b') -> NAnd a' b'
  NOr a b ->
    case (flattenNnf a, flattenNnf b) of
      (NConst True, _)  -> NConst True
      (_, NConst True)  -> NConst True
      (NConst False, t) -> t
      (t, NConst False) -> t
      (a', b') -> NOr a' b'
  x -> x


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

underTermF :: TermF Int (Bool -> Dependency -> [Dependency]) -> Bool -> Dependency -> [Dependency]
underTermF = \case
  TAnd a b -> \n d ->
    if not n then orit a b n d else andit a b n d

  TOr a b -> \n d ->
    if n then orit a b n d else andit a b n d

  TNot a -> \n d ->
    a (not n) d

  TVar a -> \n d ->
    case d of
      DFalse -> [ DLit (Literal n a) ]
      DLit (Literal t l)
        | n /\ not t -> [ DDeps l a ]
        | not n /\ t -> [ DDeps a l ]
        | otherwise  -> [ ]
          -- underapproximate
      DDeps f t
        | n     /\ a /= t -> [ ]
        | not n /\ a /= f -> [ ]
        | otherwise -> [ d ]

  TConst b -> \n d ->
    if b \/ n then [ ] else [ d ]

  where
    orit a b n d  =
      [ y | x <- a n d , y <- b n x]
    andit a b n d =
      a n d ++ b n d

underDependencies :: Term Int -> [Dependency]
underDependencies a = cata underTermF a True DFalse

overTermF :: TermF Int (Bool -> Dependency -> [Dependency]) -> Bool -> Dependency -> [Dependency]
overTermF = \case
  TAnd a b -> \n d ->
    if not n then orit a b n d else andit a b n d

  TOr a b -> \n d ->
    if n then orit a b n d else andit a b n d

  TNot a -> \n d ->
    a (not n) d

  TVar a -> \n d ->
    case d of
      DFalse -> [ DLit (Literal n a) ]
      DLit (Literal t l)
        | n /\ not t -> [ DDeps l a ]
        | not n /\ t -> [ DDeps a l ]
        | otherwise  -> [ d ]
          -- overapproximate
      _ -> [ d ]

  TConst b -> \n d ->
    if b \/ n then [ ] else [ d ]

  where
    orit a b n d  =
      concat
      [ case x of
          DDeps _ _ -> [x]
          _ -> b n x
      | x <- a n d
      ]
    andit a b n d =
      a n d ++ b n d

overDependencies :: Term Int -> [Dependency]
overDependencies a = cata overTermF a True DFalse




  -- NAnd a b ->
  --   go contex a ++ go contex b
  -- NOr a b ->

  -- NLit l ->
  --   case context of
  --     Nothing ->
  --       [ DLit l ]
  --     Just (Literal l)
  --       ->

  -- NConst True ->
  --   []

  -- NConst False ->
  --   error ""

















-- -- | We are currently operating at 31 bits.
-- type VarId = Int32

-- -- | Negation normal form
-- data Nnf
--   = NAnd Nnf Nnf
--   | NOr Nnf Nnf
--   | NVar !Bool !Int
--   | NConst !Bool
--   deriving (Eq)

-- instance BooleanAlgebra Nnf where
--   type BooleanVar Nnf = Int
--   tt = NVar True
--   ff = NVar False

-- instance Boolean Nnf where
--   (/\)  = NAnd
--   (\/)  = NOr
--   not _ = error "not supported"
--   true  = NConst True
--   false = NConst False
--   {-# INLINE (/\) #-}
--   {-# INLINE (\/) #-}
--   {-# INLINE not #-}

-- instance Show Nnf where
--   showsPrec n = \case
--     NAnd a b -> showParen (n > 5) (
--       showsPrec 5 a
--       . showString " /\\ "
--       . showsPrec 6 b
--       )
--     NOr a b -> showParen (n > 4) (
--       showsPrec 4 a
--       . showString " \\/ "
--       . showsPrec 5 b
--       )
--     NVar b i ->
--       showParen (n > 9) (showString (if b then "tt " else "ff ") . showsPrec 9 i)
--     NConst b ->
--       showParen (n > 9) (if b then showString "true" else showString "false")

-- nnfCompiler :: Term Int -> Nnf
-- nnfCompiler = go True where
--   go n = \case
--     TAnd t1 t2 ->
--       (if n then NAnd else NOr) (go n t1) (go n t2)
--     TOr  t1 t2 ->
--       (if n then NOr else NAnd) (go n t1) (go n t2)
--     TNot t1 ->
--       go (not n) t1
--     TVar a ->
--       NVar n a
--     TConst b ->
--       NConst b
-- {-# INLINE nnfCompiler #-}

-- nnfFlatCompiler :: Term Int -> Nnf
-- nnfFlatCompiler = flattenNnf . nnfCompiler

-- flattenNnf :: Nnf -> Nnf
-- flattenNnf = \case
--   NAnd a b ->
--     case (flattenNnf a, flattenNnf b) of
--       (NConst True, t)  -> t
--       (t, NConst True)  -> t
--       (NConst False, _) -> NConst False
--       (_, NConst False) -> NConst False
--       (a', b') -> NAnd a' b'
--   NOr a b ->
--     case (flattenNnf a, flattenNnf b) of
--       (NConst True, _)  -> NConst True
--       (_, NConst True)  -> NConst True
--       (NConst False, t) -> t
--       (t, NConst False) -> t
--       (a', b') -> NOr a' b'
--   x -> x

-- -- | Split a variable into it's components
-- splitVar :: VarId -> (Bool, Int32)
-- splitVar i = (not $ testBit i 31, clearBit i 31)
-- {-# INLINE splitVar #-}

-- -- | Split a variable into it's components
-- joinVar :: (Bool, Int32) -> VarId
-- joinVar (b, i) = if b then clearBit i 31 else setBit i 31
-- {-# INLINE joinVar #-}

-- notvar :: VarId -> VarId
-- notvar i = complementBit i 31
-- {-# INLINE notvar #-}

-- -- | A Clause is a unboxed sorted vector of 'Var's.
-- data Clause = Clause IS.IntSet IS.IntSet
--   deriving (Eq, Ord)

-- instance BooleanAlgebra (Literal a) where
--   type BooleanVar (Literal a) = a
--   tt a = Literal True a
--   ff a = Literal False a

-- data Literal a =
--   Literal {-# unpack #-} !Bool {-# unpack #-} !a
--   deriving (Ord, Eq)

-- instance Show a => Show (Literal a) where
--   showsPrec n (Literal b a) = showParen (n > 9)
--     (showString (if b then "tt " else "ff ") . showsPrec 9 a)

-- newtype IntLiteral = IntLiteral (Literal Int)
--   deriving (Eq, Ord, Num)

-- instance Show IntLiteral where
--   showsPrec n (IntLiteral (Literal b a)) =
--     showsPrec n ((if b then 1 else -1) * a)

-- instance BooleanAlgebra Clause where
--   type BooleanVar Clause = Int
--   tt a = Clause (IS.singleton a) mempty
--   ff a = Clause mempty (IS.singleton a)

-- instance Num a => Num (Literal a) where
--   fromInteger i =
--     Literal (signum i > 0) (abs $ fromInteger i)
--   negate (Literal b i) = (Literal (not b) i)
--   (+) = error "+ is undefined on literals"
--   (*) = error "* is undefined on literals"
--   abs (Literal _ i) = Literal True i
--   signum a = a

-- orVid :: Bool -> Int -> Clause -> Maybe Clause
-- orVid b i (Clause trs fls)
--   | b && (not $ i `IS.member` fls ) =
--     Just $ Clause (i `IS.insert` trs) fls
--   | not b && (not $ i `IS.member` trs) =
--     Just $ Clause trs (i `IS.insert` fls)
--   | otherwise =
--     Nothing

-- instance Show Clause where
--   showsPrec n (Clause tr fl) =
--     showParen (n > 9) $
--     showString "clause "
--     . showList (
--     map (IntLiteral . Literal True) (IS.toList tr)
--     ++ map (IntLiteral . Literal False) (IS.toList fl)
--     )

-- clause :: (Foldable f) => f (Bool, Int) -> Clause
-- clause (toList -> f) =
--   Clause
--   (IS.fromList . map snd $ trues)
--   (IS.fromList . map snd $ falses)
--   where
--     (trues, falses) = List.partition fst f
-- {-# INLINE clause #-}

-- negateClause :: Clause -> [Clause]
-- negateClause (Clause tr fl) =
--   [ Clause mempty (IS.singleton i) | i <- IS.toList tr]
--   ++ [ Clause (IS.singleton i) mempty | i <- IS.toList fl]
-- {-# INLINE negateClause #-}

-- joinClause :: Clause -> Clause -> Clause
-- joinClause (Clause trs1 fls1) (Clause trs2 fls2) =
--   Clause
--   (IS.union trs1 trs2)
--   (IS.union fls1 fls2)
-- {-# INLINE joinClause #-}

-- flattenClause :: Clause -> Maybe Clause
-- flattenClause a@(Clause tr fl) =
--   if tr `IS.disjoint` fl
--   then Just a
--   else Nothing

-- -- | A CNF is a list of clauses
-- type Cnf = [Clause]

-- -- instance Boolean Cnf where
-- --   a /\ b = a ++ b

-- --   (\/) = liftM2 joinClause

-- --   not = \case
-- --     [] -> false
-- --     c : rest ->
-- --      negateClause c \/ not rest

-- --   true  = []
-- --   false = [clause []]

-- cnfCompiler :: Term Int -> Cnf
-- cnfCompiler = cnfCompiler' . nnfFlatCompiler

-- cnfCompiler' :: Nnf -> Cnf
-- cnfCompiler' =
--   flip appEndo [] . go (Clause mempty mempty) (Endo . (:)) where
--   -- go: c is the dominating clause. Every clause have to be or'ed with
--   -- this. This is a fold.
--   go c k = \case
--     NAnd t1 t2   -> go c k t1 <> go c k t2
--     NOr t1 t2    -> go c (\c' -> go c' k t2) t1
--     NVar b i     ->
--       case orVid b i c of
--         Just a -> k a
--         Nothing -> mempty
--     NConst True  -> mempty
--     NConst False -> k c


-- -- go where
-- --   go = \case
-- --     TAnd t1 t2 -> go t1 ++ go t2
-- --     TOr
-- --   undefined -- \case
-- --   -- TAnd clauses ->
--   --   concatMap cnfCompiler clauses
--   -- TOr terms ->
--   --   concat $ mapM cnfCompiler (toList terms)
--   -- TNot b -> map (map (over _1 not)) $ cnfCompiler b
