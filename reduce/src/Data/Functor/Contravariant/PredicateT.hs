{-# language Rank2Types #-}
{-# language BangPatterns #-}
module Data.Functor.Contravariant.PredicateT where

-- contravariant
import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible

-- transformers
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe

-- base
import           Control.Monad
import           Control.Applicative

class CoTransMonad t where
  under :: (forall a. m a -> n a) -> t m b -> t n b
  mash :: Monad m => (a -> m b) -> t m b -> t m a

newtype PredicateT m a =
  PredicateT { runPredicateT :: a -> m Bool }

instance Applicative m => Semigroup (PredicateT m a) where
  (<>) f g = PredicateT $
    \a -> (&&) <$> runPredicateT f a <*> runPredicateT f a
  {-# inline (<>) #-}

instance Applicative m => Monoid (PredicateT m a) where
  mappend = (<>)
  mempty = PredicateT ( const . pure $ True )
  {-# inline mappend #-}
  {-# inline mempty #-}

instance Contravariant (PredicateT m) where
  contramap f g = PredicateT $ runPredicateT g . f
  {-# inline contramap #-}

instance Applicative m => Divisible (PredicateT m) where
  -- divide :: (a -> (b, c)) -> f b -> f c -> f a
  divide split fb fc =
    contramap split (contramap fst fb <> contramap snd fc)
  conquer = mempty
  {-# inline divide #-}
  {-# inline conquer #-}

instance CoTransMonad PredicateT where
  under fn pred =
    PredicateT $ \a -> fn $ runPredicateT pred a
  mash fn pred =
    PredicateT $ fn >=> runPredicateT pred

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

instance CoTransMonad GuardT where
  under fn pred =
    GuardT $ \a -> fn $ runGuardT pred a
  mash fn pred =
    GuardT $ fn >=> runGuardT pred

-- | Predicates are predicates.
ifTrueT :: Applicative m => PredicateT m Bool
ifTrueT = PredicateT $ pure
{-# inline ifTrueT #-}

-- | Get a basic guard
guardT :: Alternative m => GuardT m Bool
guardT = GuardT guard
{-# inline guardT #-}

-- | Create A GuardT from a Predicate T
asGuard :: MonadPlus m => PredicateT m a -> GuardT m a
asGuard pred =
  mash (runPredicateT pred) guardT
{-# inline asGuard #-}

liftUnder ::
  (Monad m, MonadTrans t, CoTransMonad c)
  => c m a
  -> c (t m) a
liftUnder = under lift

-- | Create A GuardT from a Predicate T
asMaybeGuard :: Monad m => PredicateT m a -> GuardT (MaybeT m) a
asMaybeGuard = asGuard . liftUnder
