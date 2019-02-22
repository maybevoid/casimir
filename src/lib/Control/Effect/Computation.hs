
module Control.Effect.Computation where

import Control.Effect.Class
import Control.Effect.Union
import Control.Effect.Empty

data Pure a (eff :: * -> *) = Pure a

pureComputation :: forall a eff .
  (Effect eff)
  => a
  -> Computation EmptyRow (Pure a) eff
pureComputation x = Computation comp
  where
    comp :: forall eff' .
      (Effect eff')
      => LiftEff eff eff'
      -> Pure a eff'
    comp _ = Pure x

liftComputation :: forall row comp eff1 eff2 .
  (EffRow row, Effect eff1, Effect eff2)
  => LiftEff eff1 eff2
  -> Computation row comp eff1
  -> Computation row comp eff2
liftComputation lift12 comp1 = Computation comp2
  where
    comp2 :: forall eff3 .
      (Effect eff3)
      => LiftEff eff2 eff3
      -> (EffConstraint row eff3 => comp eff3)
    comp2 lift23 = runComp comp1 $ joinLift lift12 lift23

withHandler
  :: forall row1 row2 comp eff1.
  ( Effect eff1
  , EffRow row1
  , EffRow row2
  )
  => row1 eff1
  -> Computation (Union row1 row2) comp eff1
  -> Computation row2 comp eff1
withHandler ops comp1 = Computation comp2
  where
    comp2 :: forall eff2 .
      (Effect eff2)
      => LiftEff eff1 eff2
      -> (EffConstraint row2 eff2 => comp eff2)
    comp2 lift12 = bindConstraint (effmap lift12 ops) comp3
      where
        comp3 :: (EffConstraint row1 eff2, EffConstraint row2 eff2) => comp eff2
        comp3 = runComp comp1 lift12

applyHandler
  :: forall row1 row2 comp eff1 eff2 .
  ( Effect eff1
  , Effect eff2
  , EffRow row1
  , EffRow row2
  )
  => row1 eff1
  -> LiftEff eff2 eff1
  -> Computation (Union row1 row2) comp eff2
  -> Computation row2 comp eff1
applyHandler ops lift21 comp1 = withHandler ops comp2
  where
    comp2 :: Computation (Union row1 row2) comp eff1
    comp2 = liftComputation lift21 comp1