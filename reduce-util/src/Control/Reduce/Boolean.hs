{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
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

-- base
import Control.Monad
import Control.Applicative
import Control.Monad.ST
import Data.Coerce
import Data.Monoid
import Data.Bool (bool)
import Data.Function
import Data.Foldable
import Data.Maybe
import Data.List.NonEmpty (nonEmpty)
import Data.STRef
import qualified Data.List as L
import GHC.Generics (Generic)
import Prelude hiding (not, and, or)
import qualified Prelude

import Debug.Trace

-- deepseq
import Control.DeepSeq

-- aseon
import Data.Aeson

-- binary
import Data.Binary

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- containers
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM

-- hashtables
import qualified Data.HashTable.ST.Cuckoo as CHT
import qualified Data.HashTable.Class as HT

-- hashable
import Data.Hashable

-- text
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Builder

-- lens
import Control.Lens hiding (andOf, orOf)

-- reduce-util
import Control.Reduce.Graph

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

class BooleanAlgebra f where
  tt :: a -> f a
  ff :: a -> f a

  var :: Bool -> a -> f a
  var True  = tt
  var False = ff


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
  deriving (Generic)

deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Ord (f (Fix f)) => Ord (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance NFData (f (Fix f)) => NFData (Fix f)

instance FromJSON (f (Fix f)) => FromJSON (Fix f) where
  parseJSON = fmap Fix . parseJSON

instance ToJSON (f (Fix f)) => ToJSON (Fix f) where
  toJSON (Fix a) = toJSON a
  toEncoding (Fix a) = toEncoding a

instance Binary (f (Fix f)) => Binary (Fix f) where
  get = Fix <$> get
  put (Fix a) = put a

class Functor f => Fixed f b | b -> f where
  cata :: (f a -> a) -> b -> a
  default cata :: Coercible (Fix f) b => (f a -> a) -> b -> a
  cata f = f . fmap (cata f) . unFix . coerce
  {-# INLINE cata #-}

  ana  :: (a -> f a) -> a -> b
  default ana :: Coercible b (Fix f) => (a -> f a) -> a -> b
  ana f = coerce . Fix . fmap (ana f) . f
  {-# INLINE ana #-}

cataM :: forall m f b a. (Monad m, Traversable f, Fixed f b) => (f a -> m a) -> b -> m a
cataM fn = cata (sequence >=> fn)
{-# INLINE cataM #-}

-- anaM :: forall m f b a. (Monad m, Traversable f, Fixed f b) => (a -> m (f a)) -> a -> m b
-- anaM fn = ana m . _ where
--   m :: (m a -> f (m a))
--   m = undefined
-- {-# INLINE anaM #-}

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
    , Generic, NFData
    , ToJSON, FromJSON , Binary
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
  deriving (Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Binary)
  deriving newtype NFData

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

instance BooleanAlgebra Term where
  tt a = liftF (TVar a)
  ff a = neg (liftF (TVar a))

showsPrecTermF :: Show a => TermF a (Int -> ShowS) -> Int -> ShowS
showsPrecTermF = \case
  TAnd a b -> \n -> showParen (n > 3) (a 3 . showString " ∧ " . b 4)
  TOr a b  -> \n -> showParen (n > 2) (a 2 . showString " ∨ " . b 3)
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

assignVars :: forall a b x. (Fixed (TermF a) x) => (a -> TermF b (Term b)) -> x -> Term b
assignVars fn = cata \case
  TAnd a b -> liftF (TAnd a b)
  TOr  a b -> liftF (TOr a b)
  TNot a   -> liftF (TNot a)
  TVar a   -> liftF (fn a)
  TConst b -> liftF (TConst b)

-- | A literal is either true or false.
data Literal a = Literal !Bool a
  deriving ( Eq, Ord, Generic, NFData
           , ToJSON, FromJSON
           , Binary, Functor, Foldable, Traversable
           )

instance BooleanAlgebra Literal where
  tt = Literal True
  ff = Literal False

instance Show a => Show (Literal a) where
  showsPrec n (Literal b i) =
    showParen (n > 9) $
    showString (if b then "tt " else "ff ") . showsPrec 10 i

-- | Negation normal form
data NnfF a f
  = NAnd f f
  | NOr f f
  | NLit !(Literal a)
  | NConst !Bool
  deriving (Show, Eq, Ord
           , Functor, Foldable
           , Traversable, Generic, NFData
           , ToJSON
           , FromJSON
           , Binary
           )

newtype Nnf a = Nnf (Fix (NnfF a))
  deriving (Eq, Ord, Generic)
  deriving newtype NFData
  deriving newtype ToJSON
  deriving newtype FromJSON

instance Fixed (NnfF a) (Nnf a)

instance Functor Nnf where
  fmap f = cata $ liftF . \case
    NAnd a b -> NAnd a b
    NOr a b -> NOr a b
    NLit l -> NLit (fmap f l)
    NConst b -> NConst b

instance Foldable Nnf where
  foldMap f = cata $ \case
    NAnd a b -> a <> b
    NOr a b -> a <> b
    NLit l -> foldMap f l
    NConst _ -> mempty

newtype NnfAsTerm a = NnfAsTerm { nnfFromTerm :: Nnf a }
  deriving (Eq, Ord)

instance Fixed (TermF a) (NnfAsTerm a) where
  -- cata :: (TermF Int a -> a) -> b -> a
  cata fn = cata (fn . handle) . nnfFromTerm where
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

instance Show a => Show (Nnf a) where
  showsPrec n f = cata showsPrecTermF (NnfAsTerm f) n

instance Show a => Show (NnfAsTerm a) where
  showsPrec n f = cata showsPrecTermF f n

instance Boolean (Nnf a) where
  a /\ b  = liftF $ NAnd a b
  a \/ b  = liftF $ NOr a b
  not     = nnfFromTerm . fromTerm . neg . toTerm . NnfAsTerm
  true    = liftF $ NConst True
  false   = liftF $ NConst False
  {-# INLINE (/\) #-}
  {-# INLINE (\/) #-}
  {-# INLINE not #-}

instance BooleanAlgebra Nnf where
  tt = liftF . NLit . tt
  ff = liftF . NLit . ff

showsPrecNnfF :: Show a => NnfF a (Int -> ShowS) -> Int -> ShowS
showsPrecNnfF = \case
  NAnd a b -> \n -> showParen (n > 3) (a 3 . showString " ∧ " . b 4)
  NOr a b  -> \n -> showParen (n > 2) (a 2 . showString " ∨ " . b 3)
  NLit (Literal b i) -> \n ->
    showParen (n > 9) (showString (if b then "tt " else "ff ") . showsPrec 10 i)
  NConst True -> const $ showString "true"
  NConst False -> const $ showString "false"


-- | Flattens an nnf
flattenNnf :: Nnf a -> Nnf a
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
data Dependency a
  = DFalse
  | DLit !(Literal a)
  | DDeps !a !a
  deriving (Eq, Ord)

infixr 5 ~~>
(~~>) :: a -> a -> Dependency a
(~~>) = DDeps

instance BooleanAlgebra Dependency where
  tt = DLit . tt
  ff = DLit . ff

instance Show a => Show (Dependency a) where
  showsPrec n = \case
    DFalse    -> showString "DFalse"
    DLit    l -> showsPrec n l
    DDeps i j -> showParen (n > 4) (showsPrec 5 i . showString " ~~> " . showsPrec 5 j)

overNnfF :: NnfF a (Dependency a -> [Dependency a]) -> Dependency a -> [Dependency a]
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

underNnfF :: Eq a => NnfF a (Dependency a -> [Dependency a]) -> Dependency a -> [Dependency a]
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
  -- TODO: This is not correct. (why?)

underDependencies :: (Fixed (NnfF a) b, Eq a) => b -> [Dependency a]
underDependencies a =
  cata underNnfF a DFalse

overDependencies :: (Fixed (NnfF a) b, Eq a) => b -> [Dependency a]
overDependencies a =
  cata overNnfF a DFalse

instance (Hashable a, Hashable b) => Hashable (NnfF a b) where
  hashWithSalt s = \case
    NAnd a b ->
      s `hashWithSalt` (0 :: Int) `hashWithSalt` a `hashWithSalt` b
    NOr a b ->
      s `hashWithSalt` (1 :: Int) `hashWithSalt` a `hashWithSalt` b
    NLit (Literal t l) ->
      s `hashWithSalt` (2 :: Int) `hashWithSalt` t `hashWithSalt` l
    NConst b ->
      s `hashWithSalt` (3 :: Int) `hashWithSalt` b
  {-# INLINE hashWithSalt #-}

data Rnnf
  = RAnd {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | ROr  {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | RLit {-# UNPACK #-} !Int
  -- ^ The value of the literal is encoded in the sign of the
  -- reference to the literal
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, Binary)

data ReducedNnf = ReducedNnf
  { redNnfHead  :: Either Bool Int
  , redNnfItems :: V.Vector Rnnf
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Binary ReducedNnf where
  get = do
    redNnfHead <- get
    redNnfItems <- case redNnfHead of
      Right n -> V.replicateM (n + 1) get
      Left _ -> return V.empty
    return ReducedNnf {..}
  put ReducedNnf {..} = do
    put redNnfHead
    case redNnfHead of
      Left _ -> return ()
      Right n -> V.mapM_ put (V.take (n+1) redNnfItems)

instance Hashable Rnnf where
  hashWithSalt s = \case
    RAnd a b ->
      s `hashWithSalt` (0 :: Int) `hashWithSalt` a `hashWithSalt` b
    ROr a b ->
      s `hashWithSalt` (1 :: Int) `hashWithSalt` a `hashWithSalt` b
    RLit l ->
      s `hashWithSalt` (2 :: Int) `hashWithSalt` l
  {-# INLINE hashWithSalt #-}

memorizer ::
  (Eq a, Hashable a)
  => (forall s. (a -> ST s Int) -> ST s b)
  -> (b, V.Vector a)
memorizer fn = runST $ do
  hashmap  <- HT.new
  countVar <- newSTRef 0
  itemsVar <- newSTRef =<< VM.new 128
    -- =<< readSTRef countVar

  b <- fn \new -> CHT.mutateST hashmap new \case
    Just k ->
      return (Just k, k)
    Nothing -> do
      count <- readSTRef countVar
      writeSTRef countVar (count+1)
      itemsM <- readSTRef itemsVar
      itemsM' <- if VM.length itemsM <= count
        then do
          itemsM' <- VM.grow itemsM count
          writeSTRef itemsVar itemsM'
          return itemsM'
        else return itemsM
      VM.unsafeWrite itemsM' count new
      return (Just count, count)

  itemsM <- readSTRef itemsVar
  items <- V.freeze itemsM

  return (b, items)
{-# INLINEABLE memorizer #-}

memorizeRnnf ::
  (forall s. (Rnnf -> ST s Int) -> ST s (Either Bool Int))
  -> ReducedNnf
memorizeRnnf fn = ReducedNnf {..} where
  ( redNnfHead :: Either Bool Int
    , V.take
      (either
        (const 0)
        (\a -> 1 + if a < 0 then a - minBound else a)
        redNnfHead
      )
    -> redNnfItems
    ) = memorizer fn
{-# INLINEABLE memorizeRnnf #-}

-- | Compress an nnf to rnnf.
compressNnfF ::
  Applicative m
  => (Rnnf -> m Int)
  -> NnfF Int (Either Bool Int)
  -> m (Either Bool Int)
compressNnfF fn = \case
  NAnd (Left False) _ ->
    pure $ Left False
  NAnd (Left True)  b ->
    pure $ b
  NAnd _ (Left False) ->
    pure $ Left False
  NAnd a (Left True) ->
    pure $ a
  NAnd (Right a) (Right b)
    | a == b ->
      pure $ Right a
    | abs (a - b) == abs minBound ->
      pure $ Left False
    | otherwise ->
      Right <$> fn (RAnd a b)

  NOr (Left False) b ->
    pure $ b
  NOr (Left True)  _ ->
    pure $ Left True
  NOr a (Left False) ->
    pure $ a
  NOr _ (Left True)  ->
    pure $ Left True
  NOr (Right a) (Right b)
    | a == b ->
      pure $ Right a
    | abs (a - b) == abs minBound ->
      pure $ Left True
    | otherwise ->
      Right <$> fn (ROr a b)

  NLit (Literal b i)  ->
    Right . (\x -> if b then x else minBound + x)
    <$> fn (RLit i)

  NConst b ->
    pure $ Left b

nnfToTerm :: Fixed (NnfF x) b => b -> Term x
nnfToTerm = cata \case
  NAnd a b -> a /\ b
  NOr a b -> a \/ b
  NLit (Literal b l) -> var b l
  NConst b -> liftF (TConst b)

  -- NLit (Literal b i)  ->
  --   Right . (\x -> if b then x else minBound + x)
  --   <$> fn (RLit i)

  -- NConst b ->
  --   pure $ Left b

reduceNnf ::
  Fixed (NnfF Int) b
  -- => (forall a. NnfF Int a -> NnfF Int a)
  => b
  -> ReducedNnf
reduceNnf b = memorizeRnnf g where
  g :: Monad m => (Rnnf -> m Int) -> m (Either Bool Int)
  g add = cataM (compressNnfF add) b
{-# INLINEABLE reduceNnf #-}

displayCNF :: Foldable t => t (IS.IntSet) -> ShowS
displayCNF =
  appEndo
  . foldMap Endo
  . L.intersperse (showString " ")
  . map displayClause
  . toList

displayClause :: IS.IntSet -> ShowS
displayClause = showParen True
  . appEndo
  . foldMap Endo
  . L.intersperse (showString " ")
  . map displayLiteral
  . IS.toList

  where
    displayLiteral i = case reindex i of
      (True, l)  -> showsPrec 0 l
      (False, l) -> showString "!" . showsPrec 0 l

clauseLearning :: S.Set (IS.IntSet) -> S.Set (IS.IntSet)
clauseLearning s = go s $ invert s where
  go items = \case
    [ ] -> items
    fct -> go x $ invert learned
      where
        learned = x S.\\ items
        x = flip S.map items $
          foldl' (\it f ->
                    if f `IS.isProperSubsetOf` it
                    then it IS.\\ f
                    else it
                 )
          `flip` fct
  invert = map (IS.map negindex) . toList


compressNnf ::
  Fixed (NnfF Int) b
  -- => (forall a. NnfF Int a -> NnfF Int a)
  => b
  -> ReducedNnf
compressNnf nnf = memorizeRnnf g where
  g :: forall m. Monad m => (Rnnf -> m Int) -> m (Either Bool Int)
  g add = do
    res <- cataM compressNnfF' nnf
    case S.minView res of
      Nothing -> pure $ Left True
      Just (fst', _)
        | IS.size fst' == 0 -> pure $ Left False
        | otherwise -> Right <$> realize res
    where
      realize :: S.Set IS.IntSet -> m Int
      realize =
        foldr1 (\x y -> add =<< RAnd <$> y <*> x)
        . map realizeOrs
        . toList . clauseLearning

      realizeOrs :: IS.IntSet -> m Int
      realizeOrs =
        foldr1 (\x y -> add =<< ROr <$> y <*> x)
        . map pure
        . IS.toList

      -- | Compress an nnf to rnnf.
      compressNnfF' ::
        NnfF Int (S.Set IS.IntSet)
        -> m (S.Set IS.IntSet)
      compressNnfF' = \case
        NAnd a b ->
          pure $ S.union a b

        NOr a b ->
          case [ IS.union a' b' | a' <- toList a, b' <- toList b ] of
            [ ] -> pure $ S.empty
            x : [  ] -> pure $ S.singleton x
            _ -> do
              i <- realize a
              j <- realize b
              S.singleton . IS.singleton <$> add (ROr i j)

        NLit (Literal b l) -> do
          i <- (\x -> if b then x else minBound + x) <$> add (RLit l)
          pure . S.singleton . IS.singleton $ i

        NConst True ->
          pure S.empty

        NConst False ->
          pure . S.singleton $ IS.empty
{-# INLINEABLE compressNnf #-}

reduceNnfSize :: ReducedNnf -> Int
reduceNnfSize ReducedNnf {..} =
  V.length redNnfItems

reindex :: Int -> (Bool, Int)
reindex n = (n >= 0, if n < 0 then n - minBound else n)

negindex :: Int -> Int
negindex n = if n < 0 then n - minBound else n + minBound

unindex :: Bool -> Int -> Int
unindex b n = if b then n + minBound else n

redIndexed :: Int -> V.Vector Rnnf -> Rnnf
redIndexed n v = V.unsafeIndex v (snd $ reindex n)

instance Fixed (NnfF Int) ReducedNnf where
  -- cata :: (f a -> a) -> b -> a
  cata fn ReducedNnf {..} =
    case redNnfHead of
      Left b -> fn (NConst b)
      Right x -> lkp x
    where
      v   = V.map red redNnfItems
      lkp (reindex -> (b, r)) =
        V.unsafeIndex v r b
      red = \case
        RAnd a b -> \_ -> fn (lkp <$> NAnd a b)
        ROr  a b -> \_ -> fn (lkp <$> NOr a b)
        RLit   t -> \b -> fn (NLit (Literal b t))
  {-# INLINEABLE cata #-}

  -- ana :: (a -> f a) -> a -> b
  ana fn a = memorizeRnnf g where
    g :: Monad m => (Rnnf -> m Int) -> m (Either Bool Int)
    g add = go a where
      go x = compressNnfF add =<< traverse go (fn x)
  {-# INLINEABLE ana #-}

showRnnf :: ReducedNnf -> String
showRnnf r = cata showsPrecNnfF r 0 ""

splitRnnf :: Int -> ReducedNnf -> ReducedNnf
splitRnnf v red = memorizeRnnf g where
  g :: (Rnnf -> ST s Int) -> ST s (Either Bool Int)
  g add = do
    (t, f) <- flip cataM red \case

      NAnd (ta, fa) (tb, fb) -> do
        x <- compressNnfF add (NAnd ta tb)
        y <- compressNnfF add (NAnd fa fb)
        return (x, y)

      NOr (ta, fa) (tb, fb) -> do
        x <- compressNnfF add (NOr ta tb)
        y <- compressNnfF add (NOr fa fb)
        return (x, y)

      NLit l@(Literal b t)
        | t == v ->
            return
              ( Left (b == False)
              , Left (b == True)
              )
        | otherwise -> do
            m <- compressNnfF add (NLit l)
            return (m, m)

      NConst b ->
        return (Left b, Left b)
       
    i <- add (RLit v)
    x <- compressNnfF add (NAnd (Right $ unindex True  i) t)
    y <- compressNnfF add (NAnd (Right $ unindex False i) f)

    compressNnfF add (NOr x y)


-- type NnfTransformer a =
--   NnfF Int a -> Either a (NnfF Int a)

conditionNnf :: Literal Int -> ReducedNnf -> ReducedNnf
conditionNnf l red = memorizeRnnf g where
  g :: (Rnnf -> ST s Int) -> ST s (Either Bool Int)
  g add = cataM (compressNnfF add . conditionNnfF l) red
{-# INLINEABLE conditionNnf #-}

-- compressNnf :: (forall a. NnfF Int a -> NnfF Int a) -> ReducedNnf -> ReducedNnf
-- compressNnf = transformNnf . compressNnfF

conditionLiteral :: Eq a => Literal a -> Literal a -> Either (Literal a) Bool
conditionLiteral (Literal b l) lit@(Literal b' l')
  | l == l' = Right (b == b')
  | otherwise = Left lit

-- | Given an Nnf condition it on a literal.
conditionNnfF :: Literal Int -> NnfF Int f -> NnfF Int f
conditionNnfF lit = \case
  NLit l -> either NLit NConst $ conditionLiteral lit l
  x -> x

-- -- | Given an Nnf condition it on a literal.
-- conditionNnf :: Literal Int -> ReducedNnf -> ReducedNnf
-- conditionNnf lit = compressNnf \case
--   NLit l -> either NLit NConst $ conditionLiteral lit l
--   x -> x

asConjunction :: ReducedNnf -> [ Nnf Int ]
asConjunction = flip appEndo [] . snd . cata \case
  NAnd (a', a) (b', b) ->
    (a' /\ b', a <> b)
  NOr (a, _) (b, _) ->
    (a \/ b, Endo ((a \/ b) :))
  NLit (Literal b l) ->
    (var b l, Endo (var b l:))
  NConst True  ->
    (true, Endo (true:))
  NConst False ->
    (false, Endo (false:))


conflictingVar :: ReducedNnf -> (Maybe Int, IS.IntSet)
conflictingVar = cata \case
  NAnd (n, s) (n', s') ->
    ( fmap maximum
      . nonEmpty
      . catMaybes
      $ [ n,  n'
        , fst <$> IS.maxView (IS.intersection s s')
        ]
    , IS.union s s'
    )
  NOr (n, s) (n', s') ->
    ( maximum <$> nonEmpty (catMaybes [ n,  n'])
    , IS.union s s'
    )
  NLit (Literal _ l) ->
    ( Nothing
    , IS.singleton l
    )
  NConst _ ->
    ( Nothing
    , IS.empty
    )

-- | To compile a Nnf to Dnnf we just have to make sure that
-- no variables are reused in ands.
compileDnnf :: ReducedNnf -> ReducedNnf
compileDnnf nnf =
  case fst $ conflictingVar nnf of
    Just v -> compileDnnf (splitRnnf v nnf)
    Nothing -> nnf

-- | Solves minSat
minSat :: ReducedNnf -> IS.IntSet
minSat = cata \case
  NAnd a b -> IS.union a b
  NOr  a b -> minimumBy (compare `on` IS.size) [a, b]
  NLit (Literal True a) -> IS.singleton a
  _ -> IS.empty

reduceNnfVars :: ReducedNnf -> V.Vector Int
reduceNnfVars =
  V.mapMaybe (\case RLit l -> Just l; _ -> Nothing) . redNnfItems

unify :: ReducedNnf -> (V.Vector IS.IntSet, ReducedNnf)
unify nnf =
  ( sccs
  , memorizeRnnf fn
  )
  where
    fn :: (Rnnf -> ST s Int) -> ST s (Either Bool Int)
    fn add = flip cataM nnf \case
      NAnd a b -> compressNnfF add (NAnd a b)
      NOr a b  -> compressNnfF add (NOr a b)
      NLit (Literal b l) ->
        compressNnfF add (NLit (Literal b (helper IM.! l)))
      NConst b ->
        compressNnfF add (NConst b)

    (graph, _) = buildGraphFromNodesAndEdges
      [ (a, a) | a <- V.toList (reduceNnfVars nnf) ]
      [ Edge () a b |  DDeps a b <- underDependencies nnf ]

    sccs = V.fromList (scc graph)

    helper = IM.fromList
      [ (nodeLabel (nodes graph V.! j), i)
      | (i, s) <- itoList sccs, j <- IS.toList s
      ]

boundVars :: ReducedNnf -> IS.IntSet
boundVars = cata \case
  NAnd a b -> a `IS.union` b
  NOr a b -> a `IS.intersection` b
  NLit (Literal b l)  -> IS.singleton (unindex b l)
  _ -> IS.empty

extractNegation :: Int -> ReducedNnf -> ReducedNnf
extractNegation i nnf = memorizeRnnf fn where
  fn :: (Rnnf -> ST s Int) -> ST s (Either Bool Int)
  fn add = do
    (xr, t) <- flip cataM nnf \a -> do
      x <- case a of
        NAnd (ax, t1) (bx, t2) -> do
          x <- mk (NAnd ax bx)
          t <- mk (NAnd t1 t2)
          pure (x, t)
        NOr (ax, t1) (bx, t2) -> do
          x  <- mk (NOr ax bx)
          ta <- mk (NOr ax t2)
          tb <- mk (NOr bx t1)
          y <- mk (NAnd x ta)
          r <- mk (NAnd y tb)
          t  <- mk (NOr t1 t2)
          pure (r, t)

        NLit (Literal b l)
          | l == i -> liftM2 (,)
            (mk $ NConst b)
            if b then
              (mk $ NLit (Literal b l))
              else (mk $ NConst True)
           
          | otherwise -> liftM2 (,)
            (mk $ NConst True)
            (mk $ NLit (Literal b l))

        NConst b -> liftM2 (,)
          (mk $ NConst True)
          (mk $ NConst b)

      trc $ show a ++ " -> " ++ show x
      return x

    trc $ "XR : " ++ show xr
    trc $ "T : " ++ show t

    k <- flip cataM nnf \case
      NAnd a b -> mk (NAnd a b)
      NOr a b -> mk (NOr a b)
      NLit (Literal b l)
        | l == i /\ b ->
          mk . NAnd xr =<< mk (NLit $ Literal b l)
        | l == i /\ not b ->
          mk (NConst True)
        | otherwise ->
          mk (NLit $ Literal b l)
      NConst b ->
        mk (NConst b)

    mk . NAnd k =<< mk . NOr xr =<< mk (NLit $ Literal False i)

    where
      mk x = do
        y <- compressNnfF add x
        trc ("MK " ++ either (bool "F" "T") show y ++ "\t<- " ++ show x)
        return y

      trc a = if True then return () else traceM a

-- | Print a reducedNnf as a dot graph
dotReducedNnf :: ReducedNnf -> LazyText.Text
dotReducedNnf red@ReducedNnf {..} =
  toLazyText $ "digraph {\n "
    <> ifoldMap handleNnf redNnfItems
    <> "}"
  where
    vars =
      flip cata red $ \case
        NAnd a b -> IS.union a b
        NOr a b -> IS.union a b
        NLit (Literal b i) -> IS.singleton (unindex b i)
        NConst _ -> IS.empty

    key (reindex -> (b, i)) =
      (if b then "t" else "f") <> fromString (show i)

    handleNnf i = \case

      RAnd a b ->
        key i <> " [label=\"∧\"]"
        <> ";\n " <> key i <> " -> " <> key a
        <> ";\n " <> key i <> " -> " <> key b
        <> ";\n "

      ROr a b ->
        key i <> " [label=\"∨\"]"
        <> ";\n " <> key i <> " -> " <> key a
        <> ";\n " <> key i <> " -> " <> key b
        <> ";\n "

      RLit l -> flip foldMap [True, False] \b ->
        if unindex b l `IS.member` vars
        then
          (if b then "t" else "f") <> fromString (show i)
          <> " [label=\""
          <> (if b then "+" else "-")
          <> fromString (show l)
          <> "\"];\n "
        else
          mempty
