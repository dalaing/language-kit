{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Pattern where

import Control.Lens

import Lang

newtype Pattern k a = Pattern { unPattern :: CoreF k (CVar a)}

makeWrapped ''Pattern

instance (CoreConstraint Eq k (CVar a)) => Eq (Pattern k a) where
  Pattern x1 == Pattern x2 = x1 == x2

instance (CoreConstraint Ord k (CVar a)) => Ord (Pattern k a) where
  compare (Pattern x1) (Pattern x2) = compare x1 x2

instance (CoreConstraint Show k (CVar a)) => Show (Pattern k a) where
  showsPrec n (Pattern x) = showsPrec n x

instance CoreConstraint1 Functor k => Functor (Pattern k) where
  fmap f (Pattern x) = Pattern $ fmap (fmap f) x

instance CoreConstraint1 Foldable k => Foldable (Pattern k) where
  foldMap f (Pattern x) = foldMap (foldMap f) x

instance CoreConstraint1 Traversable k => Traversable (Pattern k) where
  traverse f (Pattern x) = Pattern <$> traverse (traverse f) x

_PatternVar :: Prism' (Pattern k a) a
_PatternVar = _Wrapped . _CoreVar . _CPatternVar

ptVar :: a -> Pattern k a
ptVar = review _PatternVar

_PatternBody :: Prism' (Pattern k a) (PatternF k (CoreF k) (CVar a))
_PatternBody = _Wrapped . _CPattern

