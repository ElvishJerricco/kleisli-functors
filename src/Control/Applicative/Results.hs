{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Applicative.Results where

import Control.Applicative
import Data.Bifunctor
import Data.Data
import Data.Functor.Classes
import Data.Semigroup
import GHC.Generics

newtype Results e a = Results { getResults :: Either e a }
  deriving
    ( Show
    , Read
    , Ord
    , Eq
    , Show2
    , Read2
    , Ord2
    , Eq2
    , Show1
    , Read1
    , Ord1
    , Eq1
    , Generic1
    , Generic
    , Typeable
    , Data
    , Semigroup
    , Functor
    , Bifunctor
    , Foldable
    , Traversable
    )

results :: (e -> r) -> (a -> r) -> Results e a -> r
results l r (Results f) = either l r f

instance Semigroup e => Applicative (Results e) where
  pure = Results . Right
  Results (Left x) <*> Results (Left y) = Results $ Left $ x <> y
  Results f <*> Results a = Results (f <*> a)

instance (Semigroup e, Monoid e) => Alternative (Results e) where
  empty = Results $ Left mempty
  Results (Left x) <|> Results (Left y) = Results $ Left $ x <> y
  Results (Left _) <|> b = b
  a <|> _ = a
