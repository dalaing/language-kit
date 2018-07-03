{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module STLC.Rule.Eval (
    stlcRulesEval
  ) where

import Control.Lens
import Bound

import Lang
import Structure.Term
import STLC.Structure.Term
import Rule.Eval

lamValue :: HasTmSTLC k => ValueRule k a
lamValue = Value $ \tm -> do
  _ <- preview _TmLam tm
  pure tm

appTmStep :: HasTmSTLC k => StepRule k a
appTmStep = StepRecurse $ \step tm -> do
  (tmFn, tmArg) <- preview _TmApp tm
  tmFn' <- step tmFn
  pure $ review _TmApp (tmFn', tmArg)

appLamStep :: ( HasTmSTLC k
              , CoreConstraint1 Functor k
              , CoreConstraint2 Bound k
              , Eq a)
           => StepRule k a
appLamStep = Step $ \tm -> do
  (tmFn, tmArg) <- preview _TmApp tm
  (_, s) <- preview _TmLam tmFn
  pure $ instantiate1Tm tmArg s

stlcRulesEval :: ( HasTmSTLC k
                 , CoreConstraint1 Functor k
                 , CoreConstraint2 Bound k
                 , Eq a)
              => EvalRulesIn k a
stlcRulesEval =
  let
    stlcValueRules = [
        lamValue
      ]
    stlcStepRules = [
        appTmStep
      , appLamStep
      ]
  in
    EvalRulesIn stlcValueRules stlcStepRules
