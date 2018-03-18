{-|
Copyright   : (c) Dave Laing, 2018
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
module IBE where

import Control.Lens
import Bound
import Data.Functor.Classes
import Data.Deriving

import Lang

import TmInt
import TmBool
import TmEq
import TmSTLC
import TyInt
import TyBool
import TySTLC

data IBE

instance Lang IBE where
  newtype TermF IBE f a = Tm { unTm :: IBETerm f a }
    deriving (Eq, Ord, Functor, Foldable, Traversable)
  newtype TypeF IBE f a = Ty { unTy :: IBEType f a }
    deriving (Eq, Ord, Functor, Foldable, Traversable)
  data KindF IBE f a = Ki
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  data PatternF IBE f a = Pt
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  data DataF IBE f a = Dt
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Show1 f, Show (f a), Show a) => Show (TermF IBE f a) where
  showsPrec n (Tm x) = showsPrec n x

instance (Show1 f, Show (f a), Show a) => Show (TypeF IBE f a) where
  showsPrec n (Ty x) = showsPrec n x

_Tm :: Iso' (TermF IBE f a) (IBETerm f a)
_Tm = iso unTm Tm

_Ty :: Iso' (TypeF IBE f a) (IBEType f a)
_Ty = iso unTy Ty

data IBETerm f a =
    IBETmInt (TmIntF f a)
  | IBETmBool (TmBoolF f a)
  | IBETmEq (TmEqF f a)
  | IBETmSTLC (TmSTLCF f a)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

data IBEType f a =
    IBETyInt (TyIntF f a)
  | IBETyBool (TyBoolF f a)
  | IBETySTLC (TySTLCF f a)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''IBETerm
makePrisms ''IBEType

instance HasTmInt IBE where
  _TmIntF = _Tm . _IBETmInt

instance HasTmBool IBE where
  _TmBoolF = _Tm . _IBETmBool

instance HasTmEq IBE where
  _TmEqF = _Tm . _IBETmEq

instance HasTmSTLC IBE where
  _TmSTLCF = _Tm . _IBETmSTLC

instance HasTyInt IBE where
  _TyIntF = _Ty . _IBETyInt

instance HasTyBool IBE where
  _TyBoolF = _Ty . _IBETyBool

instance HasTySTLC IBE where
  _TySTLCF = _Ty . _IBETySTLC

instance (Eq1 f, Monad f) => Eq1 (IBETerm f) where
  liftEq = $(makeLiftEq ''IBETerm)

instance (Ord1 f, Monad f) => Ord1 (IBETerm f) where
  liftCompare = $(makeLiftCompare ''IBETerm)

instance (Show1 f, Show a) => Show (IBETerm f a) where
  showsPrec = showsPrec1

instance Show1 f => Show1 (IBETerm f) where
  liftShowsPrec sp sl n (IBETmInt x) = liftShowsPrec sp sl n x
  liftShowsPrec sp sl n (IBETmBool x) = liftShowsPrec sp sl n x
  liftShowsPrec sp sl n (IBETmEq x) = liftShowsPrec sp sl n x
  liftShowsPrec sp sl n (IBETmSTLC x) = liftShowsPrec sp sl n x

instance Bound IBETerm where
  IBETmInt x >>>= f = IBETmInt (x >>>= f)
  IBETmBool x >>>= f = IBETmBool (x >>>= f)
  IBETmEq x >>>= f = IBETmEq (x >>>= f)
  IBETmSTLC x >>>= f = IBETmSTLC (x >>>= f)

instance (Eq1 f, Monad f) => Eq1 (IBEType f) where
  liftEq = $(makeLiftEq ''IBEType)

instance (Ord1 f, Monad f) => Ord1 (IBEType f) where
  liftCompare = $(makeLiftCompare ''IBEType)

instance (Show1 f, Show a) => Show (IBEType f a) where
  showsPrec = showsPrec1

instance Show1 f => Show1 (IBEType f) where
  liftShowsPrec sp sl n (IBETyInt x) = liftShowsPrec sp sl n x
  liftShowsPrec sp sl n (IBETyBool x) = liftShowsPrec sp sl n x
  liftShowsPrec sp sl n (IBETySTLC x) = liftShowsPrec sp sl n x

instance Bound IBEType where
  IBETyInt x >>>= f = IBETyInt (x >>>= f)
  IBETyBool x >>>= f = IBETyBool (x >>>= f)
  IBETySTLC x >>>= f = IBETySTLC (x >>>= f)

instance (Eq1 f, Monad f) => Eq1 (TermF IBE f) where
  liftEq eq (Tm x1) (Tm x2) = liftEq eq x1 x2

instance (Ord1 f, Monad f) => Ord1 (TermF IBE f) where
  liftCompare cmp (Tm x1) (Tm x2) = liftCompare cmp x1 x2

instance Show1 f => Show1 (TermF IBE f) where
  liftShowsPrec sp sl n (Tm x) = liftShowsPrec sp sl n x

instance Bound (TermF IBE) where
  Tm x >>>= f = Tm (x >>>= f)

instance (Eq1 f, Monad f) => Eq1 (TypeF IBE f) where
  liftEq eq (Ty x1) (Ty x2) = liftEq eq x1 x2

instance (Ord1 f, Monad f) => Ord1 (TypeF IBE f) where
  liftCompare cmp (Ty x1) (Ty x2) = liftCompare cmp x1 x2

instance Show1 f => Show1 (TypeF IBE f) where
  liftShowsPrec sp sl n (Ty x) = liftShowsPrec sp sl n x

instance Bound (TypeF IBE) where
  Ty x >>>= f = Ty (x >>>= f)

instance Eq1 (KindF IBE f) where
  liftEq _ _ _ = True

instance Ord1 (KindF IBE f) where
  liftCompare _ _ _ = EQ

instance Show1 (KindF IBE f) where
  liftShowsPrec _ _ _ _ = id

instance Bound (KindF IBE) where
  _ >>>= _ = Ki

instance Eq1 (PatternF IBE f) where
  liftEq _ _ _ = True

instance Ord1 (PatternF IBE f) where
  liftCompare _ _ _ = EQ

instance Show1 (PatternF IBE f) where
  liftShowsPrec _ _ _ _ = id

instance Bound (PatternF IBE) where
  _ >>>= _ = Pt

instance Eq1 (DataF IBE f) where
  liftEq _ _ _ = True

instance Ord1 (DataF IBE f) where
  liftCompare _ _ _ = EQ

instance Show1 (DataF IBE f) where
  liftShowsPrec _ _ _ _ = id

instance Bound (DataF IBE) where
  _ >>>= _ = Dt
