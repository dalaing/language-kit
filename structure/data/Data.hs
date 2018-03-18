{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Data where

import Control.Lens

import Lang

newtype Data k a = Data { unData :: CoreF k (CVar a)}

makeWrapped ''Data

instance (CoreConstraint Eq k (CVar a)) => Eq (Data k a) where
  Data x1 == Data x2 = x1 == x2

instance (CoreConstraint Ord k (CVar a)) => Ord (Data k a) where
  compare (Data x1) (Data x2) = compare x1 x2

instance (CoreConstraint Show k (CVar a)) => Show (Data k a) where
  showsPrec n (Data x) = showsPrec n x

instance CoreConstraint1 Functor k => Functor (Data k) where
  fmap f (Data x) = Data $ fmap (fmap f) x

instance CoreConstraint1 Foldable k => Foldable (Data k) where
  foldMap f (Data x) = foldMap (foldMap f) x

instance CoreConstraint1 Traversable k => Traversable (Data k) where
  traverse f (Data x) = Data <$> traverse (traverse f) x

_DataVar :: Prism' (Data k a) a
_DataVar = _Wrapped . _CoreVar . _CDataVar

dtVar :: a -> Data k a
dtVar = review _DataVar

_DataBody :: Prism' (Data k a) (DataF k (CoreF k) (CVar a))
_DataBody = _Wrapped . _CData

