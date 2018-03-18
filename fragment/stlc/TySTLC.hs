{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module TySTLC where

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Lens
import Bound
import Data.Functor.Classes
import Data.Deriving

import Lang
import Type

data TySTLCF ty a =
  TyFArr (ty a) (ty a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TySTLCF

instance (Eq1 f, Monad f) => Eq1 (TySTLCF f) where
  liftEq = $(makeLiftEq ''TySTLCF)

instance (Ord1 f, Monad f) => Ord1 (TySTLCF f) where
  liftCompare = $(makeLiftCompare ''TySTLCF)

deriveShow1 ''TySTLCF

instance Bound TySTLCF where
  TyFArr ty1 ty2 >>>= f = TyFArr (ty1 >>= f) (ty2 >>= f)

class HasTySTLC k where
  _TySTLCF :: Prism' (TypeF k f a) (TySTLCF f a)
  _TySTLCF =
    let
      f (TyFArr x y) =
        review _TyArr' (x, y)

      g' pp pr tm = Right . review pr <$> preview pp tm
      g tm =
        fromMaybe (Left tm) . asum . fmap ($ tm) $ [
          g' _TyArr' _TyFArr
        ]
    in
      prism f g

  _TyArr' :: Prism' (TypeF k f a) (f a, f a)
  _TyArr' = _TySTLCF . _TyFArr

_TyArr :: HasTySTLC k => Prism' (Type k a) (Type k a, Type k a)
_TyArr = _TypeBody . _TyArr' . bimapping _Unwrapped _Unwrapped

tyArr :: HasTySTLC k => Type k a -> Type k a -> Type k a
tyArr x y = review _TyArr (x, y)
