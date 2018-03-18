{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Lang where

import Control.Monad (ap)

import Control.Lens
import Bound

import GHC.Exts (Constraint)

class Lang k where
  data TermF    k (f :: * -> *) a
  data TypeF    k (f :: * -> *) a
  data KindF    k (f :: * -> *) a
  data PatternF k (f :: * -> *) a
  data DataF    k (f :: * -> *) a

data CVar a =
    CKindVar a
  | CTypeVar a
  | CTermVar a
  | CPatternVar a
  | CDataVar a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''CVar

data CoreF k a =
    CoreVar a
  | CKind (KindF k (CoreF k) a)
  | CType (TypeF k (CoreF k) a)
  | CTerm (TermF k (CoreF k) a)
  | CPattern (PatternF k (CoreF k) a)
  | CData (DataF k (CoreF k) a)

makePrisms ''CoreF

type CoreConstraint (c :: * -> Constraint) k a =
  ( c (KindF k (CoreF k) a)
  , c (TypeF k (CoreF k) a)
  , c (TermF k (CoreF k) a)
  , c (PatternF k (CoreF k) a)
  , c (DataF k (CoreF k) a)
  , c a
  )

type CoreConstraint1 (c :: (* -> *) -> Constraint) k =
  ( c (KindF k (CoreF k))
  , c (TypeF k (CoreF k))
  , c (TermF k (CoreF k))
  , c (PatternF k (CoreF k))
  , c (DataF k (CoreF k))
  )

type CoreConstraint2 (c :: ((* -> *) -> * -> *) -> Constraint) k =
  ( c (KindF k)
  , c (TypeF k)
  , c (TermF k)
  , c (PatternF k)
  , c (DataF k)
  )

instance (CoreConstraint Eq k a) => Eq (CoreF k a) where
  CoreVar x1 == CoreVar x2 = x1 == x2
  CKind x1 == CKind x2 = x1 == x2
  CType x1 == CType x2 = x1 == x2
  CTerm x1 == CTerm x2 = x1 == x2
  CPattern x1 == CPattern x2 = x1 == x2
  CData x1 == CData x2 = x1 == x2
  _ == _ = False

instance (CoreConstraint Ord k a) => Ord (CoreF k a) where
  compare (CoreVar x1) (CoreVar x2) = compare x1 x2
  compare (CoreVar _) _ = LT
  compare _ (CoreVar _) = GT
  compare (CKind x1) (CKind x2) = compare x1 x2
  compare (CKind _) _ = LT
  compare _ (CKind _) = GT
  compare (CType x1) (CType x2) = compare x1 x2
  compare (CType _) _ = LT
  compare _ (CType _) = GT
  compare (CTerm x1) (CTerm x2) = compare x1 x2
  compare (CTerm _) _ = LT
  compare _ (CTerm _) = GT
  compare (CPattern x1) (CPattern x2) = compare x1 x2
  compare (CPattern _) _ = LT
  compare _ (CPattern _) = GT
  compare (CData x1) (CData x2) = compare x1 x2

instance (CoreConstraint Show k a) => Show (CoreF k a) where
  showsPrec n (CoreVar x) = showsPrec n x
  showsPrec n (CKind x) = showsPrec n x
  showsPrec n (CType x) = showsPrec n x
  showsPrec n (CTerm x) = showsPrec n x
  showsPrec n (CPattern x) = showsPrec n x
  showsPrec n (CData x) = showsPrec n x

instance CoreConstraint1 Functor k => Functor (CoreF k) where
  fmap f (CoreVar x) = CoreVar $ f x
  fmap f (CKind x) = CKind $ fmap f x
  fmap f (CType x) = CType $ fmap f x
  fmap f (CTerm x) = CTerm $ fmap f x
  fmap f (CPattern x) = CPattern $ fmap f x
  fmap f (CData x) = CData $ fmap f x

instance CoreConstraint1 Foldable k => Foldable (CoreF k) where
  foldMap f (CoreVar x) = f x
  foldMap f (CKind x) = foldMap f x
  foldMap f (CType x) = foldMap f x
  foldMap f (CTerm x) = foldMap f x
  foldMap f (CPattern x) = foldMap f x
  foldMap f (CData x) = foldMap f x

instance CoreConstraint1 Traversable k => Traversable (CoreF k) where
  traverse f (CoreVar x) = CoreVar <$> f x
  traverse f (CKind x) = CKind <$> traverse f x
  traverse f (CType x) = CType <$> traverse f x
  traverse f (CTerm x) = CTerm <$> traverse f x
  traverse f (CPattern x) = CPattern <$> traverse f x
  traverse f (CData x) = CData <$> traverse f x

instance ( CoreConstraint1 Functor k
         , CoreConstraint2 Bound k
         ) => Applicative (CoreF k) where
  pure = CoreVar
  (<*>) = ap

instance ( CoreConstraint1 Functor k
         , CoreConstraint2 Bound k
         ) => Monad (CoreF k) where
  CoreVar x >>= f = f x
  CKind x >>= f = CKind (x >>>= f)
  CType x >>= f = CType (x >>>= f)
  CTerm x >>= f = CTerm (x >>>= f)
  CPattern x >>= f = CPattern (x >>>= f)
  CData x >>= f = CData (x >>>= f)
