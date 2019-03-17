
module Control.Effect.Base.LiftEff
where

import Data.Kind

import Control.Effect.Base.Effect
import Control.Effect.Base.EffFunctor

data LiftEff (eff1 :: (Type -> Type)) (eff2 :: (Type -> Type))
  = LiftEff {
    liftEff :: eff1 ~> eff2,

    applyLift
      :: forall comp
      . (EffFunctor comp)
      => comp eff1
      -> comp eff2,

    leftJoin
      :: forall eff3
      . (Effect eff3)
      => LiftEff eff2 eff3
      -> LiftEff eff1 eff3,

    rightJoin
      :: forall eff0
      . (Effect eff0)
      => LiftEff eff0 eff1
      -> LiftEff eff0 eff2
  }

mkLiftEff
  :: forall eff1 eff2
   . (Effect eff1, Effect eff2)
  => eff1 ~> eff2
  -> LiftEff eff1 eff2
mkLiftEff lifter1 = lifter2
 where
  lifter2 = LiftEff {
    liftEff = lifter1,

    applyLift = effmap lifter1,

    leftJoin = \lifter3 ->
      rightJoin lifter3 lifter2,

    rightJoin = \lifter3 ->
      mkLiftEff (lifter1 . liftEff lifter3)
  }

idLift
  :: forall eff . (Effect eff)
  => LiftEff eff eff
idLift = LiftEff {
  liftEff = id,

  applyLift = id,

  leftJoin = id,

  rightJoin = id
}

joinLift
  :: forall eff1 eff2 eff3
   . (Effect eff1, Effect eff2, Effect eff3)
  => LiftEff eff1 eff2
  -> LiftEff eff2 eff3
  -> LiftEff eff1 eff3
joinLift = leftJoin