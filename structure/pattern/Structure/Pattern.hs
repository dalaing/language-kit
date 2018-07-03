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
module Structure.Pattern where

import Control.Lens
import Data.Functor.Classes
import Data.Deriving

import Lang

newtype Pattern k a = Pattern { unPattern :: CoreF k (CVar a)}

makeWrapped ''Pattern

instance (CoreConstraint1 Eq1 k, Eq a) => Eq (Pattern k a) where
  (==) = eq1

instance (CoreConstraint1 Eq1 k) => Eq1 (Pattern k) where
  liftEq = $(makeLiftEq ''Pattern)

instance (CoreConstraint1 Ord1 k, Ord a) => Ord (Pattern k a) where
  compare = compare1

instance (CoreConstraint1 Ord1 k) => Ord1 (Pattern k) where
  liftCompare = $(makeLiftCompare ''Pattern)

instance (CoreConstraint1 Show1 k, Show a) => Show (Pattern k a) where
  showsPrec = showsPrec1

instance (CoreConstraint1 Show1 k) => Show1 (Pattern k) where
  liftShowsPrec = $(makeLiftShowsPrec ''Pattern)

deriving instance CoreConstraint1 Functor k => Functor (Pattern k)
deriving instance CoreConstraint1 Foldable k => Foldable (Pattern k)
deriving instance CoreConstraint1 Traversable k => Traversable (Pattern k)

_PatternVar :: Prism' (Pattern k a) a
_PatternVar = _Wrapped . _CoreVar . _CPatternVar

ptVar :: a -> Pattern k a
ptVar = review _PatternVar

_PatternBody :: Prism' (Pattern k a) (PatternF k (CoreF k) (CVar a))
_PatternBody = _Wrapped . _CPattern

