{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module TmBool where

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Lens
import Bound
import Data.Functor.Classes
import Data.Deriving

import Lang
import Term

data TmBoolF tm a =
    TmFBoolLit Bool
  | TmFAnd (tm a) (tm a)
  | TmFOr (tm a) (tm a)
  | TmFNot (tm a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmBoolF

instance (Eq1 f, Monad f) => Eq1 (TmBoolF f) where
  liftEq = $(makeLiftEq ''TmBoolF)

instance (Ord1 f, Monad f) => Ord1 (TmBoolF f) where
  liftCompare = $(makeLiftCompare ''TmBoolF)

deriveShow1 ''TmBoolF

instance Bound TmBoolF where
  TmFBoolLit i >>>= _ = TmFBoolLit i
  TmFAnd tm1 tm2 >>>= f = TmFAnd (tm1 >>= f) (tm2 >>= f)
  TmFOr tm1 tm2 >>>= f = TmFOr (tm1 >>= f) (tm2 >>= f)
  TmFNot tm1 >>>= f = TmFNot (tm1 >>= f)

class HasTmBool k where
  _TmBoolF :: Prism' (TermF k f a) (TmBoolF f a)
  _TmBoolF =
    let
      f (TmFBoolLit x) =
        review _TmBoolLit' x
      f (TmFAnd x y) =
        review _TmAnd' (x, y)
      f (TmFOr x y) =
        review _TmOr' (x, y)
      f (TmFNot x) =
        review _TmNot' x

      g' pp pr tm = Right . review pr <$> preview pp tm
      g tm =
        fromMaybe (Left tm) . asum . fmap ($ tm) $ [
          g' _TmBoolLit' _TmFBoolLit
        , g' _TmAnd' _TmFAnd
        , g' _TmOr' _TmFOr
        , g' _TmNot' _TmFNot
        ]
    in
      prism f g

  _TmBoolLit' :: Prism' (TermF k f a) Bool
  _TmBoolLit' = _TmBoolF . _TmFBoolLit

  _TmAnd' :: Prism' (TermF k f a) (f a, f a)
  _TmAnd' = _TmBoolF . _TmFAnd

  _TmOr' :: Prism' (TermF k f a) (f a, f a)
  _TmOr' = _TmBoolF . _TmFOr

  _TmNot' :: Prism' (TermF k f a) (f a)
  _TmNot' = _TmBoolF . _TmFNot

_TmBoolLit :: HasTmBool k => Prism' (Term k a) Bool
_TmBoolLit = _TermBody . _TmBoolLit'

_TmAnd :: HasTmBool k => Prism' (Term k a) (Term k a, Term k a)
_TmAnd = _TermBody . _TmAnd'. bimapping _Unwrapped _Unwrapped

_TmOr :: HasTmBool k => Prism' (Term k a) (Term k a, Term k a)
_TmOr = _TermBody . _TmOr'. bimapping _Unwrapped _Unwrapped

_TmNot :: HasTmBool k => Prism' (Term k a) (Term k a)
_TmNot = _TermBody . _TmNot' . _Unwrapped

tmBoolLit :: HasTmBool k => Bool -> Term k a
tmBoolLit = review _TmBoolLit

tmAnd :: HasTmBool k => Term k a -> Term k a -> Term k a
tmAnd x y = review _TmAnd (x, y)

tmOr :: HasTmBool k => Term k a -> Term k a -> Term k a
tmOr x y = review _TmOr (x, y)

tmNot :: HasTmBool k => Term k a -> Term k a
tmNot = review _TmNot

