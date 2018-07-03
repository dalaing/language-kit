{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module STLC.Structure.Term where

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Lens
import Bound
import Data.Functor.Classes
import Data.Deriving

import Lang
import Structure.Term
import Structure.Type

data TmSTLCF tm a =
    TmFApp (tm a) (tm a)
  | TmFLam (tm a) (Scope () tm a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmSTLCF

instance (Eq1 f, Monad f) => Eq1 (TmSTLCF f) where
  liftEq = $(makeLiftEq ''TmSTLCF)

instance (Ord1 f, Monad f) => Ord1 (TmSTLCF f) where
  liftCompare = $(makeLiftCompare ''TmSTLCF)

deriveShow1 ''TmSTLCF

instance Bound TmSTLCF where
  TmFApp tm1 tm2 >>>= f = TmFApp (tm1 >>= f) (tm2 >>= f)
  TmFLam ty s >>>= f = TmFLam (ty >>= f) (s >>>= f)

class HasTmSTLC k where
  _TmSTLCF :: Prism' (TermF k f a) (TmSTLCF f a)
  _TmSTLCF =
    let
      f (TmFApp x y) =
        review _TmApp' (x, y)
      f (TmFLam x y) =
        review _TmLam' (x, y)

      g' pp pr tm = Right . review pr <$> preview pp tm
      g tm =
        fromMaybe (Left tm) . asum . fmap ($ tm) $ [
          g' _TmApp' _TmFApp
        , g' _TmLam' _TmFLam
        ]
    in
      prism f g

  _TmApp' :: Prism' (TermF k f a) (f a, f a)
  _TmApp' = _TmSTLCF . _TmFApp

  _TmLam' :: Prism' (TermF k f a) (f a, Scope () f a)
  _TmLam' = _TmSTLCF . _TmFLam


_TmApp :: HasTmSTLC k => Prism' (Term k a) (Term k a, Term k a)
_TmApp = _TermBody . _TmApp' . bimapping _Unwrapped _Unwrapped

_TmLam :: HasTmSTLC k => Prism' (Term k a) (Type k a, Scope () (CoreF k) (CVar a))
_TmLam = _TermBody . _TmLam' . bimapping _Unwrapped id

tmApp :: HasTmSTLC k => Term k a -> Term k a -> Term k a
tmApp x y = review _TmApp (x, y)

tmLam :: ( HasTmSTLC k
         , CoreConstraint1 Functor k
         , CoreConstraint2 Bound k
         , Eq a
         ) => a -> Type k a -> Term k a -> Term k a
tmLam a ty tm = review _TmLam (ty, abstract1Tm a tm)
