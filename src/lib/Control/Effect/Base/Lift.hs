
module Control.Effect.Base.Lift
where

import Control.Natural (type (~>))

type LiftEff eff1 eff2 = eff1 ~> eff2

joinLift :: forall eff1 eff2 eff3 .
  LiftEff eff1 eff2
  -> LiftEff eff2 eff3
  -> LiftEff eff1 eff3
joinLift lift12 lift23 = lift23 . lift12
