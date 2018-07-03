{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Bool.Rule.Eval (
    boolRulesEval
  ) where

import Control.Lens

import Bool.Structure.Term
import Rule.Eval

tmBoolLitValue :: HasTmBool k => ValueRule k a
tmBoolLitValue = Value $ \tm -> do
  _ <- preview _TmBoolLit tm
  pure $ tm

andTmStep :: HasTmBool k => StepRule k a
andTmStep = StepRecurse $ \step tm -> do
  (tm1, tm2) <- preview _TmAnd tm
  tm1' <- step tm1
  pure $ review _TmAnd (tm1', tm2)

andFalseStep :: HasTmBool k => StepRule k a
andFalseStep = Step $ \tm -> do
  (tm1, _) <- preview _TmAnd tm
  b <- preview _TmBoolLit tm1
  if (b == False)
  then pure tm1
  else Nothing

andTrueStep :: HasTmBool k => StepRule k a
andTrueStep = Step $ \tm -> do
  (tm1, tm2) <- preview _TmAnd tm
  b <- preview _TmBoolLit tm1
  if (b == True)
  then pure tm2
  else Nothing

orTmStep :: HasTmBool k => StepRule k a
orTmStep = StepRecurse $ \step tm -> do
  (tm1, tm2) <- preview _TmOr tm
  tm1' <- step tm1
  pure $ review _TmOr (tm1', tm2)

orFalseStep :: HasTmBool k => StepRule k a
orFalseStep = Step $ \tm -> do
  (tm1, tm2) <- preview _TmOr tm
  b <- preview _TmBoolLit tm1
  if (b == False)
  then pure tm2
  else Nothing

orTrueStep :: HasTmBool k => StepRule k a
orTrueStep = Step $ \tm -> do
  (tm1, _) <- preview _TmOr tm
  b <- preview _TmBoolLit tm1
  if (b == True)
  then pure tm1
  else Nothing

notTmStep :: HasTmBool k => StepRule k a
notTmStep = StepRecurse $ \step tm -> do
  tm' <- preview _TmNot tm
  tm'' <- step tm'
  pure $ review _TmNot tm''

notBoolStep :: HasTmBool k => StepRule k a
notBoolStep = Step $ \tm -> do
  tm' <- preview _TmNot tm
  b <- preview _TmBoolLit tm'
  pure $ review _TmBoolLit (not b)

boolRulesEval :: HasTmBool k => EvalRulesIn k a
boolRulesEval =
  let
    boolValueRules = [
        tmBoolLitValue
      ]
    boolStepRules = [
        andTmStep
      , andFalseStep
      , andTrueStep
      , orTmStep
      , orFalseStep
      , orTrueStep
      , notTmStep
      , notBoolStep
      ]
  in
    EvalRulesIn boolValueRules boolStepRules
