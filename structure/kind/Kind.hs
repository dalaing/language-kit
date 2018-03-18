{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
module Kind where

import Control.Lens
import Data.Functor.Classes
import Data.Deriving

import Lang

newtype Kind k a = Kind { unKind :: CoreF k (CVar a)}

makeWrapped ''Kind

instance (CoreConstraint1 Eq1 k, Eq a) => Eq (Kind k a) where
  (==) = eq1

instance (CoreConstraint1 Eq1 k) => Eq1 (Kind k) where
  liftEq = $(makeLiftEq ''Kind)

instance (CoreConstraint1 Ord1 k, Ord a) => Ord (Kind k a) where
  compare = compare1

instance (CoreConstraint1 Ord1 k) => Ord1 (Kind k) where
  liftCompare = $(makeLiftCompare ''Kind)

instance (CoreConstraint1 Show1 k, Show a) => Show (Kind k a) where
  showsPrec = showsPrec1

instance (CoreConstraint1 Show1 k) => Show1 (Kind k) where
  liftShowsPrec = $(makeLiftShowsPrec ''Kind)

deriving instance CoreConstraint1 Functor k => Functor (Kind k)
deriving instance CoreConstraint1 Foldable k => Foldable (Kind k)
deriving instance CoreConstraint1 Traversable k => Traversable (Kind k)

_KindVar :: Prism' (Kind k a) a
_KindVar = _Wrapped . _CoreVar . _CKindVar

kiVar :: a -> Kind k a
kiVar = review _KindVar

_KindBody :: Prism' (Kind k a) (KindF k (CoreF k) (CVar a))
_KindBody = _Wrapped . _CKind

