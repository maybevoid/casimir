
module Control.Effect.Dynamic.Handler
where

import Control.Effect.Base
import Control.Effect.Computation
import Control.Effect.Dynamic.Class
import Control.Effect.Dynamic.Lift

applyDynamic
  :: forall f ops handler comp eff1 .
  ( Effect eff1
  , EffOps ops
  , EffOps handler
  , EffFunctor comp
  , DynamicOps handler
  , EffFunctor (WrapComp comp f)
  , WrapConstraint comp f
  )
  => DynamicHandler f ops handler eff1
  -> Computation (Union handler ops) comp eff1
  -> Computation ops (WrapComp comp f) eff1
applyDynamic (DynamicHandler handler1) comp1 = Computation comp2
 where
  comp2
    :: forall eff2 .
    (Effect eff2)
    => LiftEff eff1 eff2
    -> Operation ops eff2
    -> WrapComp comp f eff2
  comp2 liftEff ops1 = comp4
   where
    handler2 :: forall a . OpsHandler handler a (f a) eff2
    handler2 = runComp handler1 liftEff ops1

    ops2 :: Operation ops (DynamicEff handler eff2)
    ops2 = effmap liftReturn ops1

    comp3 :: comp (DynamicEff handler eff2)
    comp3 = runComp comp1 (liftReturn . liftEff) (UnionOps dynamicOps ops2)

    comp4 :: WrapComp comp f eff2
    comp4 = wrapEff runDynamic comp3

    runDynamic
      :: forall a .
      DynamicEff handler eff2 a
      -> eff2 (f a)
    runDynamic eff = runDynamicEff eff handler2

handleDynamic
  :: forall ops eff a b .
  ( Effect eff
  , EffOps ops
  , DynamicOps ops
  )
  => OpsHandler ops a b eff
  -> (OpsConstraint ops (DynamicEff ops eff)
      => DynamicEff ops eff a)
  -> eff b
handleDynamic handler comp1 = runDynamicEff comp2 handler
 where
  ops :: Operation ops (DynamicEff ops eff)
  ops = dynamicOps

  comp2 :: DynamicEff ops eff a
  comp2 = bindConstraint ops comp1
