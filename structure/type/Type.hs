{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Type where

import Control.Lens

import Lang

newtype Type k a = Type { unType :: CoreF k (CVar a)}

makeWrapped ''Type

instance (CoreConstraint Eq k (CVar a)) => Eq (Type k a) where
  Type x1 == Type x2 = x1 == x2

instance (CoreConstraint Ord k (CVar a)) => Ord (Type k a) where
  compare (Type x1) (Type x2) = compare x1 x2

instance (CoreConstraint Show k (CVar a)) => Show (Type k a) where
  showsPrec n (Type x) = showsPrec n x

instance CoreConstraint1 Functor k => Functor (Type k) where
  fmap f (Type x) = Type $ fmap (fmap f) x

instance CoreConstraint1 Foldable k => Foldable (Type k) where
  foldMap f (Type x) = foldMap (foldMap f) x

instance CoreConstraint1 Traversable k => Traversable (Type k) where
  traverse f (Type x) = Type <$> traverse (traverse f) x

_TypeVar :: Prism' (Type k a) a
_TypeVar = _Wrapped . _CoreVar . _CTypeVar

tyVar :: a -> Type k a
tyVar = review _TypeVar

_TypeBody :: Prism' (Type k a) (TypeF k (CoreF k) (CVar a))
_TypeBody = _Wrapped . _CType
