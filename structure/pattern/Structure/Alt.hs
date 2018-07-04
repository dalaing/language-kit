{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Structure.Alt where

import Bound
import Control.Lens
import Data.Functor.Classes
import Data.Deriving

import Lang
import Structure.Pattern

data Alt k a =
  Alt {
    _altPattern :: Pattern k a
  , _altBody :: Scope Int (CoreF k) (CVar a)
  }

makeLenses ''Alt

instance (CoreConstraint1 Eq1 k, CoreConstraint1 Functor k, CoreConstraint2 Bound k, Eq a) => Eq (Alt k a) where
  (==) = eq1

instance (CoreConstraint1 Eq1 k, CoreConstraint1 Functor k, CoreConstraint2 Bound k) => Eq1 (Alt k) where
  liftEq = $(makeLiftEq ''Alt)

instance (CoreConstraint1 Ord1 k, CoreConstraint1 Functor k, CoreConstraint2 Bound k, Ord a) => Ord (Alt k a) where
  compare = compare1

instance (CoreConstraint1 Ord1 k, CoreConstraint1 Functor k, CoreConstraint2 Bound k) => Ord1 (Alt k) where
  liftCompare = $(makeLiftCompare ''Alt)

instance (CoreConstraint1 Show1 k, Show a) => Show (Alt k a) where
  showsPrec = showsPrec1

instance (CoreConstraint1 Show1 k) => Show1 (Alt k) where
  liftShowsPrec = $(makeLiftShowsPrec ''Alt)

deriving instance CoreConstraint1 Functor k => Functor (Alt k)
deriving instance CoreConstraint1 Foldable k => Foldable (Alt k)
deriving instance CoreConstraint1 Traversable k => Traversable (Alt k)
