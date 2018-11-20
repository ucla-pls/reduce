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

newtype GuardT m a =
  GuardT { runGuardT :: a -> m () }

instance Applicative m => Semigroup (GuardT m a) where
  (<>) f g = GuardT $
    \a -> (const $ const ()) <$> runGuardT f a <*> runGuardT f a
  {-# inline (<>) #-}

instance Applicative m => Monoid (GuardT m a) where
  mappend = (<>)
  mempty = GuardT ( const . pure $ ())
  {-# inline mappend #-}
  {-# inline mempty #-}

instance Contravariant (GuardT m) where
  contramap f g = GuardT $ runGuardT g . f
  {-# inline contramap #-}

instance Applicative m => Divisible (GuardT m) where
  divide split fb fc =
    contramap split (contramap fst fb <> contramap snd fc)
  conquer = mempty
  {-# inline divide #-}
  {-# inline conquer #-}

instance MonadFunctor GuardT where
  mmap fn pred =
    GuardT $ \a -> fn $ runGuardT pred a

instance ContravariantM GuardT where
  contramapM fn pred =
    GuardT $ fn >=> runGuardT pred

-- | Predicates are predicates.
ifTrueT :: Applicative m => PredicateM m Bool
ifTrueT = PredicateM $ pure
{-# inline ifTrueT #-}

-- | Get a basic guard
guardT :: Alternative m => GuardT m Bool
guardT = GuardT guard
{-# inline guardT #-}

-- | Create A GuardT from a Predicate T
asGuard :: MonadPlus m => PredicateM m a -> GuardT m a
asGuard pred =
  contramapM (runPredicateM pred) guardT
{-# inline asGuard #-}

liftUnder ::
  (Monad m, MonadTrans t, MonadFunctor c)
  => c m a
  -> c (t m) a
liftUnder = mmap lift

-- | Create A GuardT from a Predicate T
asMaybeGuard :: Monad m => PredicateM m a -> GuardT (MaybeT m) a
asMaybeGuard = asGuard . liftUnder
