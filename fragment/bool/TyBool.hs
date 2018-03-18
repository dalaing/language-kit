{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module TyBool where

import Control.Lens
import Bound
import Data.Functor.Classes
import Data.Deriving

import Lang
import Type

data TyBoolF (ty :: * -> *) a =
    TyFBool
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyBoolF

instance (Eq1 f, Monad f) => Eq1 (TyBoolF f) where
  liftEq = $(makeLiftEq ''TyBoolF)

instance (Ord1 f, Monad f) => Ord1 (TyBoolF f) where
  liftCompare = $(makeLiftCompare ''TyBoolF)

deriveShow1 ''TyBoolF

instance Bound TyBoolF where
  TyFBool >>>= _ = TyFBool

class HasTyBool k where
  _TyBoolF :: Prism' (TypeF k f a) (TyBoolF f a)
  _TyBoolF =
    let
      f _ =
        review _TyBool' ()
      g ty =
        case preview _TyBool' ty of
          Just _ -> Right TyFBool
          Nothing -> Left ty
    in
      prism f g

  _TyBool' :: Prism' (TypeF k f a) ()
  _TyBool' = _TyBoolF . _TyFBool

_TyBool :: HasTyBool k => Prism' (Type k a) ()
_TyBool = _TypeBody . _TyBool'

tyBool :: HasTyBool k => Type k a
tyBool = review _TyBool ()

