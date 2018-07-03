{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Int.Structure.Term where

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Lens
import Bound
import Data.Functor.Classes
import Data.Deriving

import Lang
import Structure.Term

data TmIntF tm a =
    TmFIntLit Int
  | TmFAdd (tm a) (tm a)
  | TmFSub (tm a) (tm a)
  | TmFMul (tm a) (tm a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmIntF

instance (Eq1 f, Monad f) => Eq1 (TmIntF f) where
  liftEq = $(makeLiftEq ''TmIntF)

instance (Ord1 f, Monad f) => Ord1 (TmIntF f) where
  liftCompare = $(makeLiftCompare ''TmIntF)

deriveShow1 ''TmIntF

instance Bound TmIntF where
  TmFIntLit i >>>= _ = TmFIntLit i
  TmFAdd tm1 tm2 >>>= f = TmFAdd (tm1 >>= f) (tm2 >>= f)
  TmFSub tm1 tm2 >>>= f = TmFSub (tm1 >>= f) (tm2 >>= f)
  TmFMul tm1 tm2 >>>= f = TmFMul (tm1 >>= f) (tm2 >>= f)

class HasTmInt k where
  _TmIntF :: Prism' (TermF k f a) (TmIntF f a)
  _TmIntF =
    let
      f (TmFIntLit x) =
        review _TmIntLit' x
      f (TmFAdd x y) =
        review _TmAdd' (x, y)
      f (TmFSub x y) =
        review _TmSub' (x, y)
      f (TmFMul x y) =
        review _TmMul' (x, y)

      g' pp pr tm = Right . review pr <$> preview pp tm
      g tm =
        fromMaybe (Left tm) . asum . fmap ($ tm) $ [
          g' _TmIntLit' _TmFIntLit
        , g' _TmAdd' _TmFAdd
        , g' _TmSub' _TmFSub
        , g' _TmMul' _TmFMul
        ]
    in
      prism f g

  _TmIntLit' :: Prism' (TermF k f a) Int
  _TmIntLit' = _TmIntF . _TmFIntLit

  _TmAdd' :: Prism' (TermF k f a) (f a, f a)
  _TmAdd' = _TmIntF . _TmFAdd

  _TmSub' :: Prism' (TermF k f a) (f a, f a)
  _TmSub' = _TmIntF . _TmFSub

  _TmMul' :: Prism' (TermF k f a) (f a, f a)
  _TmMul' = _TmIntF . _TmFMul

_TmIntLit :: HasTmInt k => Prism' (Term k a) Int
_TmIntLit = _TermBody . _TmIntLit'

_TmAdd :: HasTmInt k => Prism' (Term k a) (Term k a, Term k a)
_TmAdd = _TermBody . _TmAdd' . bimapping _Unwrapped _Unwrapped

_TmSub :: HasTmInt k => Prism' (Term k a) (Term k a, Term k a)
_TmSub = _TermBody . _TmSub' . bimapping _Unwrapped _Unwrapped

_TmMul :: HasTmInt k => Prism' (Term k a) (Term k a, Term k a)
_TmMul = _TermBody . _TmMul' . bimapping _Unwrapped _Unwrapped

tmIntLit :: HasTmInt k => Int -> Term k a
tmIntLit = review _TmIntLit

tmAdd :: HasTmInt k => Term k a -> Term k a -> Term k a
tmAdd x y = review _TmAdd (x, y)

tmSub :: HasTmInt k => Term k a -> Term k a -> Term k a
tmSub x y = review _TmSub (x, y)

tmMul :: HasTmInt k => Term k a -> Term k a -> Term k a
tmMul x y = review _TmMul (x, y)
