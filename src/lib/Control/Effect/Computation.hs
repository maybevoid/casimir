
module Control.Effect.Computation where

import Control.Effect.Class
import Control.Effect.Union
import Control.Effect.Ops.NoOp

data Pure a (eff :: * -> *) = Pure a

pureComputation :: forall a eff .
  (Effect eff)
  => a
  -> Computation NoOp (Pure a) eff
pureComputation x = Computation comp
  where
    comp :: forall eff' .
      (Effect eff')
      => LiftEff eff eff'
      -> Pure a eff'
    comp _ = Pure x

liftComputation :: forall ops comp eff1 eff2 .
  (EffOps ops, Effect eff1, Effect eff2)
  => LiftEff eff1 eff2
  -> Computation ops comp eff1
  -> Computation ops comp eff2
liftComputation lift12 comp1 = Computation comp2
  where
    comp2 :: forall eff3 .
      (Effect eff3)
      => LiftEff eff2 eff3
      -> (EffConstraint ops eff3 => comp eff3)
    comp2 lift23 = runComp comp1 $ joinLift lift12 lift23

withHandler
  :: forall ops1 ops2 comp eff1.
  ( Effect eff1
  , EffOps ops1
  , EffOps ops2
  )
  => ops1 eff1
  -> Computation (Union ops1 ops2) comp eff1
  -> Computation ops2 comp eff1
withHandler ops comp1 = Computation comp2
  where
    comp2 :: forall eff2 .
      (Effect eff2)
      => LiftEff eff1 eff2
      -> (EffConstraint ops2 eff2 => comp eff2)
    comp2 lift12 = bindConstraint (effmap lift12 ops) comp3
      where
        comp3 :: (EffConstraint ops1 eff2, EffConstraint ops2 eff2) => comp eff2
        comp3 = runComp comp1 lift12

applyHandler
  :: forall ops1 ops2 comp eff1 eff2 .
  ( Effect eff1
  , Effect eff2
  , EffOps ops1
  , EffOps ops2
  )
  => ops1 eff1
  -> LiftEff eff2 eff1
  -> Computation (Union ops1 ops2) comp eff2
  -> Computation ops2 comp eff1
applyHandler ops lift21 comp1 = withHandler ops comp2
  where
    comp2 :: Computation (Union ops1 ops2) comp eff1
    comp2 = liftComputation lift21 comp1