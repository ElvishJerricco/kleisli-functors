{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Kleisli.Adjunction where

import Control.Kleisli.Functor
import Control.Monad
import Data.Functor.Identity

-- | @'CoKleisliFunctor'@s that are left adjoint to
--   @'KleisliFunctor'@s. These are useful for constructing values of
--   @f@ and @g@ and converting them into monadic @m@ effects.
class (CoKleisliFunctor m g, KleisliFunctor m f) =>
      KleisliAdjunction m g f | g f -> m where
  unit :: a -> f (g a)
  counit :: g (f a) -> m a
  leftAdjunct :: (g a -> m b) -> a -> f b
  rightAdjunct :: (a -> f b) -> g a -> m b
  unit = leftAdjunct return
  counit = rightAdjunct id
  leftAdjunct f = kmap f . unit
  rightAdjunct f = counit <=< cokmap f

-- | Use an adjunction with @'Identity'@ to form an isomorphism
--   between @f@ and @m@. See also: @'coRunAdjunct'@.
runAdjunct
  :: KleisliAdjunction m Identity f
  => f a -> m a
runAdjunct = counit . Identity

-- | The inverse of @'runAdjunct'@.
coRunAdjunct
  :: KleisliAdjunction m Identity f
  => m a -> f a
coRunAdjunct = leftAdjunct runIdentity

-- | The monad formed by a @'KleisliAdjunction'@. Note, the dual
--   comonad for this would exist in @Kleisli m@, and doesn't appear
--   to be very useful.
newtype KleisliMonad m g f a = KleisliMonad
  { runKleisliMonad :: f (g a)
  }

instance (CoKleisliFunctor m g, KleisliFunctor m f) =>
         Functor (KleisliMonad m g f) where
  fmap f (KleisliMonad a) = KleisliMonad $ kmap @m (cokmap f) a

instance KleisliAdjunction m g f =>
         Applicative (KleisliMonad m g f) where
  pure = KleisliMonad . unit
  (<*>) = ap

instance KleisliAdjunction m g f =>
         Monad (KleisliMonad m g f) where
  KleisliMonad a >>= k =
    KleisliMonad $ kmap (counit <=< cokmap (runKleisliMonad . k)) a
