{-# language Rank2Types #-}
{-# language BangPatterns #-}
module Data.Functor.Contravariant.PredicateM where

-- contravariant
import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible

-- transformers
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe

-- base
import           Control.Monad
import           Control.Applicative


-- | A functor over the category of monads.
class MonadFunctor t where
  mmap :: (forall a. m a -> n a) -> t m b -> t n b

-- m a >>= (a -> m b) -> m b
-- m a >=< (b -> m a) -> m b

class ContravariantM t where
  -- | Performs actions forward but
  contramapM ::
    Monad m => (a -> m b) -> t m b -> t m a

newtype PredicateM m a =
  PredicateM { runPredicateM :: a -> m Bool }

instance Applicative m => Semigroup (PredicateM m a) where
  (<>) f g = PredicateM $
    \a -> (&&) <$> runPredicateM f a <*> runPredicateM f a
  {-# inline (<>) #-}

instance Applicative m => Monoid (PredicateM m a) where
  mappend = (<>)
  mempty = PredicateM ( const . pure $ True )
  {-# inline mappend #-}
  {-# inline mempty #-}


instance Contravariant (PredicateM m) where
  contramap f g = PredicateM $ runPredicateM g . f
  {-# inline contramap #-}

instance Applicative m => Divisible (PredicateM m) where
  -- divide :: (a -> (b, c)) -> f b -> f c -> f a
  divide split fb fc =
    contramap split (contramap fst fb <> contramap snd fc)
  conquer = mempty
  {-# inline divide #-}
  {-# inline conquer #-}


instance MonadFunctor PredicateM where
  mmap fn pred =
    PredicateM $ \a -> fn $ runPredicateM pred a

instance ContravariantM PredicateM where
  contramapM fn pred =
    PredicateM $ fn >=> runPredicateM pred

newtype GuardM m a =
  GuardM { runGuardM :: a -> m () }

instance Applicative m => Semigroup (GuardM m a) where
  (<>) f g = GuardM $
    \a -> (const $ const ()) <$> runGuardM f a <*> runGuardM f a
  {-# inline (<>) #-}

instance Applicative m => Monoid (GuardM m a) where
  mappend = (<>)
  mempty = GuardM ( const . pure $ ())
  {-# inline mappend #-}
  {-# inline mempty #-}

instance Contravariant (GuardM m) where
  contramap f g = GuardM $ runGuardM g . f
  {-# inline contramap #-}

instance Applicative m => Divisible (GuardM m) where
  divide split fb fc =
    contramap split (contramap fst fb <> contramap snd fc)
  conquer = mempty
  {-# inline divide #-}
  {-# inline conquer #-}

instance MonadFunctor GuardM where
  mmap fn pred =
    GuardM $ \a -> fn $ runGuardM pred a

instance ContravariantM GuardM where
  contramapM fn pred =
    GuardM $ fn >=> runGuardM pred

-- | Predicates are predicates.
ifTrueT :: Applicative m => PredicateM m Bool
ifTrueT = PredicateM $ pure
{-# inline ifTrueT #-}

-- | Get a basic guard
guardT :: Alternative m => GuardM m Bool
guardT = GuardM guard
{-# inline guardT #-}

-- | Create A GuardM from a Predicate T
asGuard :: MonadPlus m => PredicateM m a -> GuardM m a
asGuard pred =
  contramapM (runPredicateM pred) guardT
{-# inline asGuard #-}

liftUnder ::
  (Monad m, MonadTrans t, MonadFunctor c)
  => c m a
  -> c (t m) a
liftUnder = mmap lift

-- | Create A GuardM from a Predicate T
asMaybeGuard :: Monad m => PredicateM m a -> GuardM (MaybeT m) a
asMaybeGuard = asGuard . liftUnder
