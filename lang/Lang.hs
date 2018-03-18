{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Lang where

import Control.Monad (ap)

import Control.Lens
import Bound
import Data.Functor.Classes
import Data.Deriving

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

deriveEq1 ''CVar
deriveOrd1 ''CVar
deriveShow1 ''CVar

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

instance (CoreConstraint1 Eq1 k, Eq a) => Eq (CoreF k a) where
  (==) = eq1

instance CoreConstraint1 Eq1 k => Eq1 (CoreF k) where
  liftEq = $(makeLiftEq ''CoreF)

instance (CoreConstraint1 Ord1 k, Ord a) => Ord (CoreF k a) where
  compare = compare1

instance (CoreConstraint1 Ord1 k) => Ord1 (CoreF k) where
  liftCompare = $(makeLiftCompare ''CoreF)

instance (CoreConstraint1 Show1 k, Show a) => Show (CoreF k a) where
  showsPrec = showsPrec1

instance (CoreConstraint1 Show1 k) => Show1 (CoreF k) where
  liftShowsPrec sp _ n (CoreVar x) = sp n x
  liftShowsPrec sp sl n (CKind x) = liftShowsPrec sp sl n x
  liftShowsPrec sp sl n (CType x) = liftShowsPrec sp sl n x
  liftShowsPrec sp sl n (CTerm x) = liftShowsPrec sp sl n x
  liftShowsPrec sp sl n (CPattern x) = liftShowsPrec sp sl n x
  liftShowsPrec sp sl n (CData x) = liftShowsPrec sp sl n x

deriving instance CoreConstraint1 Functor k => Functor (CoreF k)
deriving instance CoreConstraint1 Foldable k => Foldable (CoreF k)
deriving instance CoreConstraint1 Traversable k => Traversable (CoreF k)

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
