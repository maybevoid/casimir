
module Control.Effect.Base.EffFunctor
where

import Control.Effect.Base.Lift
import Control.Effect.Base.Effect

class EffFunctor (ops :: (* -> *) -> *) where
  effmap :: forall eff1 eff2 .
    (Effect eff1, Effect eff2)
    => LiftEff eff1 eff2
    -> ops eff1
    -> ops eff2
