{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Kind where

import Control.Lens

import Lang

newtype Kind k a = Kind { unKind :: CoreF k (CVar a)}

makeWrapped ''Kind

instance (CoreConstraint Eq k (CVar a)) => Eq (Kind k a) where
  Kind x1 == Kind x2 = x1 == x2

instance (CoreConstraint Ord k (CVar a)) => Ord (Kind k a) where
  compare (Kind x1) (Kind x2) = compare x1 x2

instance (CoreConstraint Show k (CVar a)) => Show (Kind k a) where
  showsPrec n (Kind x) = showsPrec n x

instance CoreConstraint1 Functor k => Functor (Kind k) where
  fmap f (Kind x) = Kind $ fmap (fmap f) x

instance CoreConstraint1 Foldable k => Foldable (Kind k) where
  foldMap f (Kind x) = foldMap (foldMap f) x

instance CoreConstraint1 Traversable k => Traversable (Kind k) where
  traverse f (Kind x) = Kind <$> traverse (traverse f) x

_KindVar :: Prism' (Kind k a) a
_KindVar = _Wrapped . _CoreVar . _CKindVar

kiVar :: a -> Kind k a
kiVar = review _KindVar

_KindBody :: Prism' (Kind k a) (KindF k (CoreF k) (CVar a))
_KindBody = _Wrapped . _CKind

