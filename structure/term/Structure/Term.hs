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
module Structure.Term where

import Control.Lens
import Bound
import Data.Functor.Classes
import Data.Deriving

import Lang

newtype Term k a = Term { unTerm :: CoreF k (CVar a)}

makeWrapped ''Term

instance (CoreConstraint1 Eq1 k, Eq a) => Eq (Term k a) where
  (==) = eq1

instance (CoreConstraint1 Eq1 k) => Eq1 (Term k) where
  liftEq = $(makeLiftEq ''Term)

instance (CoreConstraint1 Ord1 k, Ord a) => Ord (Term k a) where
  compare = compare1

instance (CoreConstraint1 Ord1 k) => Ord1 (Term k) where
  liftCompare = $(makeLiftCompare ''Term)

instance (CoreConstraint1 Show1 k, Show a) => Show (Term k a) where
  showsPrec = showsPrec1

instance (CoreConstraint1 Show1 k) => Show1 (Term k) where
  liftShowsPrec = $(makeLiftShowsPrec ''Term)

deriving instance CoreConstraint1 Functor k => Functor (Term k)
deriving instance CoreConstraint1 Foldable k => Foldable (Term k)
deriving instance CoreConstraint1 Traversable k => Traversable (Term k)

_TermVar :: Prism' (Term k a) a
_TermVar = _Wrapped . _CoreVar . _CTermVar

tmVar :: a -> Term k a
tmVar = review _TermVar

_TermBody :: Prism' (Term k a) (TermF k (CoreF k) (CVar a))
_TermBody = _Wrapped . _CTerm

abstractTm :: ( CoreConstraint1 Functor k
              , CoreConstraint2 Bound k
              ) => (a -> Maybe b) -> Term k a -> Scope b (CoreF k) (CVar a)
abstractTm f (Term tm) =
  abstract (\x -> preview _CTermVar x >>= f) tm

abstract1Tm :: ( CoreConstraint1 Functor k
               , CoreConstraint2 Bound k
               , Eq a
               ) => a -> Term k a -> Scope () (CoreF k) (CVar a)
abstract1Tm x tm =
  abstractTm (\y -> if x == y then Just () else Nothing) tm

instantiateTm :: ( CoreConstraint1 Functor k
                 , CoreConstraint2 Bound k
                 ) => (b -> Term k a) -> Scope b (CoreF k) (CVar a) -> Term k a
instantiateTm f =
  Term . instantiate (view _Wrapped . f)

instantiate1Tm :: ( CoreConstraint1 Functor k
                  , CoreConstraint2 Bound k
                  ) => Term k a -> Scope () (CoreF k) (CVar a) -> Term k a
instantiate1Tm tm =
  instantiateTm (const tm)
