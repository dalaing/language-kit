{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Rule.Eval (
    ValueFn
  , ValueRule(..)
  , StepFn
  , StepRule(..)
  , EvalRulesIn(..)
  , EvalRulesOut(..)
  , EvalFn
  , mkEval
  ) where

import Data.Foldable (asum)
import Data.Semigroup (Semigroup(..))

import Structure.Term

type ValueFn k a = Term k a -> Maybe (Term k a)

data ValueRule k a =
    Value (ValueFn k a)
  | ValueRecurse (ValueFn k a -> ValueFn k a)

fixValueRule :: ValueFn k a -> ValueRule k a -> ValueFn k a
fixValueRule _ (Value f) = f
fixValueRule vFn (ValueRecurse f) = f vFn

mkValue :: [ValueRule k a] -> ValueFn k a
mkValue rules =
  let
    go tm = asum . fmap (\r -> fixValueRule go r tm) $ rules
  in
    go

type StepFn k a = Term k a -> Maybe (Term k a)

data StepRule k a =
    Step (StepFn k a)
  | StepRecurse (StepFn k a -> StepFn k a)

fixStepRule :: StepFn k a -> StepRule k a -> StepFn k a
fixStepRule _ (Step f) = f
fixStepRule sFn (StepRecurse f) = f sFn

mkStep :: [StepRule k a] -> StepFn k a
mkStep rules =
  let
    go tm = asum . fmap (\r -> fixStepRule go r tm) $ rules
  in
    go

data EvalRulesIn k a =
  EvalRulesIn {
    valueRules :: [ValueRule k a]
  , stepRules :: [StepRule k a]
  }

instance Semigroup (EvalRulesIn k a) where
  EvalRulesIn v1 s1 <> EvalRulesIn v2 s2 =
    EvalRulesIn (v1 <> v2) (s1 <> s2)

instance Monoid (EvalRulesIn k a) where
  mempty =
    EvalRulesIn mempty mempty
  mappend =
    (<>)

type EvalFn k a = Term k a -> Term k a

data EvalRulesOut k a =
  EvalRulesOut {
    valueFn :: ValueFn k a
  , stepFn :: StepFn k a
  , evalFn :: EvalFn k a
  }

mkEval :: EvalRulesIn k a -> EvalRulesOut k a
mkEval (EvalRulesIn vrs srs) =
  let
    v = mkValue vrs
    s = mkStep srs
    e tm = maybe tm e . s $ tm
  in
    EvalRulesOut v s e
