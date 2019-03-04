
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
    -> ((OpsConstraint ops eff') => comp eff')
}

data Handler ops handler eff1 eff2
  = Handler
    (LiftEff eff2 eff1)
    (Computation ops (Operation handler) eff1)
