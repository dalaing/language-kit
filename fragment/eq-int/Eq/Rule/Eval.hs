{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Eq.Rule.Eval (
    eqRulesEval
  ) where

import Control.Lens

import Eq.Structure.Term
import Int.Structure.Term
import Bool.Structure.Term
import Rule.Eval

eqTmTmStep :: HasTmEq k => StepRule k a
eqTmTmStep = StepRecurse $ \step tm -> do
  (tm1, tm2) <- preview _TmEq tm
  tm1' <- step tm1
  pure $ review _TmEq (tm1', tm2)

eqIntTmStep :: (HasTmEq k, HasTmInt k) => StepRule k a
eqIntTmStep = StepRecurse $ \step tm -> do
  (tm1, tm2) <- preview _TmEq tm
  _ <- preview _TmIntLit tm1
  tm2' <- step tm2
  pure $ review _TmEq (tm1, tm2')

eqIntIntStep :: (HasTmEq k, HasTmInt k, HasTmBool k) => StepRule k a
eqIntIntStep = Step $ \tm -> do
  (tm1, tm2) <- preview _TmEq tm
  i1 <- preview _TmIntLit tm1
  i2 <- preview _TmIntLit tm2
  pure $ review _TmBoolLit (i1 == i2)

neqTmTmStep :: HasTmEq k => StepRule k a
neqTmTmStep = StepRecurse $ \step tm -> do
  (tm1, tm2) <- preview _TmEq tm
  tm1' <- step tm1
  pure $ review _TmEq (tm1', tm2)

neqIntTmStep :: (HasTmEq k, HasTmInt k) => StepRule k a
neqIntTmStep = StepRecurse $ \step tm -> do
  (tm1, tm2) <- preview _TmEq tm
  _ <- preview _TmIntLit tm1
  tm2' <- step tm2
  pure $ review _TmEq (tm1, tm2')

neqIntIntStep :: (HasTmEq k, HasTmInt k, HasTmBool k) => StepRule k a
neqIntIntStep = Step $ \tm -> do
  (tm1, tm2) <- preview _TmEq tm
  i1 <- preview _TmIntLit tm1
  i2 <- preview _TmIntLit tm2
  pure $ review _TmBoolLit (i1 /= i2)

eqRulesEval :: (HasTmEq k, HasTmInt k, HasTmBool k) => EvalRulesIn k a
eqRulesEval =
  let
    eqValueRules = []
    eqStepRules = [
        eqTmTmStep
      , eqIntTmStep
      , eqIntIntStep
      , neqTmTmStep
      , neqIntTmStep
      , neqIntIntStep
      ]
  in
    EvalRulesIn eqValueRules eqStepRules
