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
module Structure.Type where

import Control.Lens
import Data.Functor.Classes
import Data.Deriving

import Lang

newtype Type k a = Type { unType :: CoreF k (CVar a)}

makeWrapped ''Type

instance (CoreConstraint1 Eq1 k, Eq a) => Eq (Type k a) where
  (==) = eq1

instance (CoreConstraint1 Eq1 k) => Eq1 (Type k) where
  liftEq = $(makeLiftEq ''Type)

instance (CoreConstraint1 Ord1 k, Ord a) => Ord (Type k a) where
  compare = compare1

instance (CoreConstraint1 Ord1 k) => Ord1 (Type k) where
  liftCompare = $(makeLiftCompare ''Type)

instance (CoreConstraint1 Show1 k, Show a) => Show (Type k a) where
  showsPrec = showsPrec1

instance (CoreConstraint1 Show1 k) => Show1 (Type k) where
  liftShowsPrec = $(makeLiftShowsPrec ''Type)

deriving instance CoreConstraint1 Functor k => Functor (Type k)
deriving instance CoreConstraint1 Foldable k => Foldable (Type k)
deriving instance CoreConstraint1 Traversable k => Traversable (Type k)

_TypeVar :: Prism' (Type k a) a
_TypeVar = _Wrapped . _CoreVar . _CTypeVar

tyVar :: a -> Type k a
tyVar = review _TypeVar

_TypeBody :: Prism' (Type k a) (TypeF k (CoreF k) (CVar a))
_TypeBody = _Wrapped . _CType
