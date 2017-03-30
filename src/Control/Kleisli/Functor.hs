{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Kleisli.Functor where

import Control.Concurrent.Async
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Sum
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Traversable
import Data.Validation

-- | @f@ is a Kleisli functor of @m@ if @f@ represents a functor from
--   @Kleisli m@ to @Hask@.
--
--   Conceptually, these are regular @'Functor'@s, except with the
--   added ability to absorb the monad @m@. For example, @'[]'@ is a
--   Kleisli functor of @'Maybe'@, because elements can be removed
--   from the list by mapping them to @'Nothing'@.
--
--   The laws are the category theoretic functor laws:
--
-- @
-- -- Identity
-- kmap return = id
-- -- Composition
-- kmap (g <=< f) = kmap g . kmap f
-- @
class (Monad m, Functor f) =>
      KleisliFunctor m f where
  kmap :: (a -> m b) -> f a -> f b

-- | Any monad is a Kleisli functor of itself.
instance Monad m =>
         KleisliFunctor m m where
  kmap = (=<<)

instance Monad m =>
         KleisliFunctor m (Const a) where
  kmap _ (Const c) = Const c

-- | Functor composition. To compose with a Kleisli functor, you need
--   either an endofunctor on Hask, or an endofunctor on @Kleisli m@.
--   This instance chooses to compose with endofunctors on Hask.
instance (KleisliFunctor m f, Functor g) =>
         KleisliFunctor m (Compose g f) where
  kmap f (Compose a) = Compose $ kmap f <$> a

instance (KleisliFunctor m f, KleisliFunctor m g) =>
         KleisliFunctor m (Product f g) where
  kmap f (Pair x y) = Pair (kmap f x) (kmap f y)

instance (KleisliFunctor m f, KleisliFunctor m g) =>
         KleisliFunctor m (Sum f g) where
  kmap f (InL a) = InL (kmap f a)
  kmap f (InR a) = InR (kmap f a)

-- | Types that are isomorphic always form Kleisli functors.
instance KleisliFunctor (Either e) (AccValidation e) where
  kmap _ (AccFailure e) = AccFailure e
  kmap f (AccSuccess a) =
    case f a of
      Left e -> AccFailure e
      Right b -> AccSuccess b

instance KleisliFunctor (Either e) (Validation e) where
  kmap _ (Failure e) = Failure e
  kmap f (Success a) =
    case f a of
      Left e -> Failure e
      Right b -> Success b

instance KleisliFunctor IO Concurrently where
  kmap f (Concurrently a) = Concurrently (a >>= f)

instance KleisliFunctor Maybe (Map k) where
  kmap = Map.mapMaybe

instance KleisliFunctor Maybe IntMap where
  kmap = IntMap.mapMaybe

instance KleisliFunctor Maybe Seq where
  kmap f = foldr (maybe id (Seq.<|) . f) Seq.empty

instance KleisliFunctor Maybe [] where
  kmap = Maybe.mapMaybe

-- | When one monad is a subset of another, they tend to form Kleisli
--   functors.
instance KleisliFunctor NonEmpty [] where
  kmap f a = a >>= NonEmpty.toList . f

-- | Like @'traverse'@, but transforms @m@ into @f@ before doing
--   applicative composition. This allows you to do monadic
--   computations and have them seamlessly converted into @f@, which
--   is useful when @f@'s @'Applicative'@ instance has advantages over
--   @m@'s. For example, @'AccValidation'@ will preserve all the
--   errors thrown by @'Either'@, instead of short circuiting after
--   the first failure. As another example, @'Concurrently'@ will run
--   each iteration's @'IO'@ effects concurrently.
--
--   With the @Monad m => KleisliFunctor m m@ instance, this works on
--   any monad without conversions.
ktraverse
  :: (Traversable t, KleisliFunctor m f, Applicative f)
  => (a -> m b)
  -> t a
  -> f (t b)
ktraverse f = traverse (kmap f . pure)

-- | See @'ktraverse'@ and @'for'@.
kfor
  :: (Traversable t, KleisliFunctor m f, Applicative f)
  => t a
  -> (a -> m b)
  -> f (t b)
kfor t f = for t (kmap f . pure)

-- | See @'ktraverse'@ and @'sequenceA'@.
ksequence
  :: (Traversable t, KleisliFunctor m f, Applicative f) => t (m a) -> f (t a)
ksequence = sequenceA . fmap (kmap id . pure)

-- | See @'ktraverse'@ and @'traverse_'@.
ktraverse_
  :: (Foldable t, KleisliFunctor m f, Applicative f)
  => (a -> m b)
  -> t a
  -> f ()
ktraverse_ f = traverse_ (kmap f . pure)

-- | See @'ktraverse'@ and @'for_'@.
kfor_
  :: (Foldable t, KleisliFunctor m f, Applicative f)
  => t a
  -> (a -> m b)
  -> f ()
kfor_ t f = for_ t (kmap f . pure)

-- | See @'ktraverse'@ and @'sequenceA_'@.
ksequence_ :: (Foldable t, KleisliFunctor m f, Applicative f) => t (m a) -> f ()
ksequence_ = foldr f (pure ()) where f a b = kmap id (pure a) *> b

-- | @g@ is a co-Kleisli-functor of @m@ if @g@ represent a functor
--   from @Hask@ to @Kleisli m@. This is the dual of
--   @'KleisliFunctor'@.
--
--   Laws:
--
-- @
-- -- Identity
-- cokmap id = return
-- -- Composition
-- cokmap (g . f) = cokmap g <=< cokmap f
-- @
class Monad m =>
      CoKleisliFunctor m g where
  cokmap :: (a -> b) -> g a -> m (g b)

instance Monad m =>
         CoKleisliFunctor m Identity where
  cokmap f (Identity a) = return . Identity $ f a
