
module Control.Effect.Base.Lift
where

type LiftEff eff1 eff2 = forall a . eff1 a -> eff2 a

joinLift :: forall eff1 eff2 eff3 .
  LiftEff eff1 eff2
  -> LiftEff eff2 eff3
  -> LiftEff eff1 eff3
joinLift lift12 lift23 = lift23 . lift12
