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
module IBE where

import Control.Lens

import Lang

import TmInt
import TmBool
import TmEq
import TyInt
import TyBool

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

instance Show (f a) => Show (TermF IBE f a) where
  showsPrec n (Tm x) = showsPrec n x

instance Show (f a) => Show (TypeF IBE f a) where
  showsPrec n (Ty x) = showsPrec n x

_Tm :: Iso' (TermF IBE f a) (IBETerm f a)
_Tm = iso unTm Tm

_Ty :: Iso' (TypeF IBE f a) (IBEType f a)
_Ty = iso unTy Ty

data IBETerm f a =
    IBETmInt (TmIntF f a)
  | IBETmBool (TmBoolF f a)
  | IBETmEq (TmEqF f a)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

data IBEType f a =
    IBETyInt (TyIntF f a)
  | IBETyBool (TyBoolF f a)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

makePrisms ''IBETerm
makePrisms ''IBEType

instance Show (f a) => Show (IBETerm f a) where
  showsPrec n (IBETmInt x) = showsPrec n x
  showsPrec n (IBETmBool x) = showsPrec n x
  showsPrec n (IBETmEq x) = showsPrec n x

instance Show (f a) => Show (IBEType f a) where
  showsPrec n (IBETyInt x) = showsPrec n x
  showsPrec n (IBETyBool x) = showsPrec n x

instance HasTmInt IBE where
  _TmIntF = _Tm . _IBETmInt

instance HasTmBool IBE where
  _TmBoolF = _Tm . _IBETmBool

instance HasTmEq IBE where
  _TmEqF = _Tm . _IBETmEq

instance HasTyInt IBE where
  _TyIntF = _Ty . _IBETyInt

instance HasTyBool IBE where
  _TyBoolF = _Ty . _IBETyBool
