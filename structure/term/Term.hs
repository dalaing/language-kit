{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Term where

import Control.Lens
import Bound

import Lang

newtype Term k a = Term { unTerm :: CoreF k (CVar a)}

makeWrapped ''Term

instance (CoreConstraint Eq k (CVar a)) => Eq (Term k a) where
  Term x1 == Term x2 = x1 == x2

instance (CoreConstraint Ord k (CVar a)) => Ord (Term k a) where
  compare (Term x1) (Term x2) = compare x1 x2

instance (CoreConstraint Show k (CVar a)) => Show (Term k a) where
  showsPrec n (Term x) = showsPrec n x

instance CoreConstraint1 Functor k => Functor (Term k) where
  fmap f (Term x) = Term $ fmap (fmap f) x

instance CoreConstraint1 Foldable k => Foldable (Term k) where
  foldMap f (Term x) = foldMap (foldMap f) x

instance CoreConstraint1 Traversable k => Traversable (Term k) where
  traverse f (Term x) = Term <$> traverse (traverse f) x

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
