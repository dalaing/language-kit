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
module Data where

import Control.Lens
import Data.Functor.Classes
import Data.Deriving

import Lang

newtype Data k a = Data { unData :: CoreF k (CVar a)}

makeWrapped ''Data

instance (CoreConstraint1 Eq1 k, Eq a) => Eq (Data k a) where
  (==) = eq1

instance (CoreConstraint1 Eq1 k) => Eq1 (Data k) where
  liftEq = $(makeLiftEq ''Data)

instance (CoreConstraint1 Ord1 k, Ord a) => Ord (Data k a) where
  compare = compare1

instance (CoreConstraint1 Ord1 k) => Ord1 (Data k) where
  liftCompare = $(makeLiftCompare ''Data)

instance (CoreConstraint1 Show1 k, Show a) => Show (Data k a) where
  showsPrec = showsPrec1

instance (CoreConstraint1 Show1 k) => Show1 (Data k) where
  liftShowsPrec = $(makeLiftShowsPrec ''Data)

deriving instance CoreConstraint1 Functor k => Functor (Data k)
deriving instance CoreConstraint1 Foldable k => Foldable (Data k)
deriving instance CoreConstraint1 Traversable k => Traversable (Data k)

_DataVar :: Prism' (Data k a) a
_DataVar = _Wrapped . _CoreVar . _CDataVar

dtVar :: a -> Data k a
dtVar = review _DataVar

_DataBody :: Prism' (Data k a) (DataF k (CoreF k) (CVar a))
_DataBody = _Wrapped . _CData

