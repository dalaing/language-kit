{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module TyInt where

import Control.Lens
import Bound
import Data.Functor.Classes
import Data.Deriving

import Lang
import Type

data TyIntF (ty :: * -> *) a =
    TyFInt
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyIntF

instance (Eq1 f, Monad f) => Eq1 (TyIntF f) where
  liftEq = $(makeLiftEq ''TyIntF)

instance (Ord1 f, Monad f) => Ord1 (TyIntF f) where
  liftCompare = $(makeLiftCompare ''TyIntF)

deriveShow1 ''TyIntF

instance Bound TyIntF where
  TyFInt >>>= _ = TyFInt

class HasTyInt k where
  _TyIntF :: Prism' (TypeF k f a) (TyIntF f a)
  _TyIntF =
    let
      f _ =
        review _TyInt' ()
      g ty =
        case preview _TyInt' ty of
          Just _ -> Right TyFInt
          Nothing -> Left ty
    in
      prism f g

  _TyInt' :: Prism' (TypeF k f a) ()
  _TyInt' = _TyIntF . _TyFInt

_TyInt :: HasTyInt k => Prism' (Type k a) ()
_TyInt = _TypeBody . _TyInt'

tyInt :: HasTyInt k => Type k a
tyInt = review _TyInt ()

