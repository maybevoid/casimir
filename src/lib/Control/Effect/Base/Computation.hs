
module Control.Effect.Base.Computation
where

import Control.Effect.Base.Lift
import Control.Effect.Base.Effect
import Control.Effect.Base.EffOps
import Control.Effect.Base.FreeEff

data Computation ops comp eff = Computation {
  runComp :: forall eff' .
    (EffOps ops, Effect eff, Effect eff')
    => LiftEff eff eff'
    -> ((EffConstraint ops eff') => comp eff')
}

data Handler ops handler outerEff innerEff
  = Handler
    (LiftEff innerEff outerEff)
    (Computation ops (Operation handler) outerEff)
