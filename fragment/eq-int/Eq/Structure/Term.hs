{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Eq.Structure.Term where

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Lens
import Bound
import Data.Functor.Classes
import Data.Deriving

import Lang
import Structure.Term

data TmEqF tm a =
    TmFEq (tm a) (tm a)
  | TmFNeq (tm a) (tm a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmEqF

instance (Eq1 f, Monad f) => Eq1 (TmEqF f) where
  liftEq = $(makeLiftEq ''TmEqF)

instance (Ord1 f, Monad f) => Ord1 (TmEqF f) where
  liftCompare = $(makeLiftCompare ''TmEqF)

deriveShow1 ''TmEqF

instance Bound TmEqF where
  TmFEq tm1 tm2 >>>= f = TmFEq (tm1 >>= f) (tm2 >>= f)
  TmFNeq tm1 tm2 >>>= f = TmFNeq (tm1 >>= f) (tm2 >>= f)

class HasTmEq k where
  _TmEqF :: Prism' (TermF k f a) (TmEqF f a)
  _TmEqF =
    let
      f (TmFEq x y) =
        review _TmEq' (x, y)
      f (TmFNeq x y) =
        review _TmNeq' (x, y)

      g' pp pr tm = Right . review pr <$> preview pp tm
      g tm =
        fromMaybe (Left tm) . asum . fmap ($ tm) $ [
          g' _TmEq' _TmFEq
        , g' _TmNeq' _TmFNeq
        ]
    in
      prism f g

  _TmEq' :: Prism' (TermF k f a) (f a, f a)
  _TmEq' = _TmEqF . _TmFEq

  _TmNeq' :: Prism' (TermF k f a) (f a, f a)
  _TmNeq' = _TmEqF . _TmFNeq

_TmEq :: HasTmEq k => Prism' (Term k a) (Term k a, Term k a)
_TmEq = _TermBody . _TmEq' . bimapping _Unwrapped _Unwrapped

_TmNeq :: HasTmEq k => Prism' (Term k a) (Term k a, Term k a)
_TmNeq = _TermBody . _TmNeq' . bimapping _Unwrapped _Unwrapped

tmEq :: HasTmEq k => Term k a -> Term k a -> Term k a
tmEq x y = review _TmEq (x, y)

tmNeq :: HasTmEq k => Term k a -> Term k a -> Term k a
tmNeq x y = review _TmNeq (x, y)
