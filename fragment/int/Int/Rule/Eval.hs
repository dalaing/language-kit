{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Int.Rule.Eval (
    intRulesEval
  ) where

import Control.Lens

import Int.Structure.Term
import Rule.Eval

tmIntLitValue :: HasTmInt k => ValueRule k a
tmIntLitValue = Value $ \tm -> do
  _ <- preview _TmIntLit tm
  pure $ tm

addTmTmStep :: HasTmInt k => StepRule k a
addTmTmStep = StepRecurse $ \step tm -> do
  (tm1, tm2) <- preview _TmAdd tm
  tm1' <- step tm1
  pure $ review _TmAdd (tm1', tm2)

addIntTmStep :: HasTmInt k => StepRule k a
addIntTmStep = StepRecurse $ \step tm -> do
  (tm1, tm2) <- preview _TmAdd tm
  _ <- preview _TmIntLit tm1
  tm2' <- step tm2
  pure $ review _TmAdd (tm1, tm2')

addIntIntStep :: HasTmInt k => StepRule k a
addIntIntStep = Step $ \tm -> do
  (tm1, tm2) <- preview _TmAdd tm
  i1 <- preview _TmIntLit tm1
  i2 <- preview _TmIntLit tm2
  pure $ review _TmIntLit (i1 + i2)

subTmTmStep :: HasTmInt k => StepRule k a
subTmTmStep = StepRecurse $ \step tm -> do
  (tm1, tm2) <- preview _TmSub tm
  tm1' <- step tm1
  pure $ review _TmSub (tm1', tm2)

subIntTmStep :: HasTmInt k => StepRule k a
subIntTmStep = StepRecurse $ \step tm -> do
  (tm1, tm2) <- preview _TmSub tm
  _ <- preview _TmIntLit tm1
  tm2' <- step tm2
  pure $ review _TmSub (tm1, tm2')

subIntIntStep :: HasTmInt k => StepRule k a
subIntIntStep = Step $ \tm -> do
  (tm1, tm2) <- preview _TmSub tm
  i1 <- preview _TmIntLit tm1
  i2 <- preview _TmIntLit tm2
  pure $ review _TmIntLit (i1 - i2)

mulTmTmStep :: HasTmInt k => StepRule k a
mulTmTmStep = StepRecurse $ \step tm -> do
  (tm1, tm2) <- preview _TmMul tm
  tm1' <- step tm1
  pure $ review _TmMul (tm1', tm2)

mulIntTmStep :: HasTmInt k => StepRule k a
mulIntTmStep = StepRecurse $ \step tm -> do
  (tm1, tm2) <- preview _TmMul tm
  _ <- preview _TmIntLit tm1
  tm2' <- step tm2
  pure $ review _TmMul (tm1, tm2')

mulIntIntStep :: HasTmInt k => StepRule k a
mulIntIntStep = Step $ \tm -> do
  (tm1, tm2) <- preview _TmMul tm
  i1 <- preview _TmIntLit tm1
  i2 <- preview _TmIntLit tm2
  pure $ review _TmIntLit (i1 * i2)

intRulesEval :: HasTmInt k => EvalRulesIn k a
intRulesEval =
  let
    intValueRules = [
        tmIntLitValue
      ]
    intStepRules = [
        addTmTmStep
      , addIntTmStep
      , addIntIntStep
      , subTmTmStep
      , subIntTmStep
      , subIntIntStep
      , mulTmTmStep
      , mulIntTmStep
      , mulIntIntStep
      ]
  in
    EvalRulesIn intValueRules intStepRules
