
module Control.Effect.Dynamic.Handler
where

import Control.Effect.Base
import Control.Effect.Computation
import Control.Effect.Dynamic.Class
import Control.Effect.Dynamic.Lift

mkDynamicArrow
  :: forall a b ops handler eff1 .
  ( Effect eff1
  , EffOps ops
  , EffOps handler
  , DynamicOps handler
  )
  => (forall eff2 .
      (Effect eff2, OpsConstraint ops eff2)
      => OpsHandler handler a b eff2)
  -> ArrowComputation ops handler a b eff1
mkDynamicArrow handler1 = Computation comp1
 where
  comp1
    :: forall eff2 .
    (Effect eff2)
    => LiftEff eff1 eff2
    -> Operation ops eff2
    -> Arrow handler a b eff2
  comp1 _ ops1 = Arrow arrow1
   where
    arrow1 :: Computation handler (Return a) eff2 -> eff2 b
    arrow1 comp2 = runDynamicEff comp3 handler2
     where
      comp3 :: DynamicEff handler eff2 a
      comp3 = returnVal $ runComp comp2 liftDynamicEff dynamicOps

      handler2 :: OpsHandler handler a b eff2
      handler2 = bindConstraint ops1 handler1

bindDynamicHandler
  :: forall f ops handler comp eff1 .
  ( Effect eff1
  , EffOps ops
  , EffOps handler
  , EffFunctor comp
  , DynamicOps handler
  )
  => DynamicHandler f ops handler eff1
  -> Computation (Union handler ops) comp eff1
  -> Computation ops comp eff1
bindDynamicHandler (DynamicHandler handler1) comp1 = Computation comp2
 where
  comp2
    :: forall eff2 .
    (Effect eff2)
    => LiftEff eff1 eff2
    -> Operation ops eff2
    -> comp eff2
  comp2 liftEff ops1 = comp4
   where
    context :: DynamicContext f ops handler eff2
    context = runComp handler1 liftEff ops1

    handler2 :: forall a . OpsHandler handler a (f a) eff2
    (DynamicContext handler2 _) = context

    extract :: forall a . eff2 (f a) -> eff2 a
    (DynamicContext _ extract) = context

    ops2 :: Operation ops (DynamicEff handler eff2)
    ops2 = effmap liftDynamicEff ops1

    comp3 :: comp (DynamicEff handler eff2)
    comp3 = runComp comp1 (liftDynamicEff . liftEff) (UnionOps dynamicOps ops2)

    comp4 :: comp eff2
    comp4 = effmap runDynamic comp3

    runDynamic
      :: forall a .
      DynamicEff handler eff2 a
      -> eff2 a
    runDynamic eff =
      extract $ runDynamicEff eff handler2

withOpsHandler
  :: forall ops eff a b .
  ( Effect eff
  , EffOps ops
  , DynamicOps ops
  )
  => OpsHandler ops a b eff
  -> (OpsConstraint ops (DynamicEff ops eff)
      => DynamicEff ops eff a)
  -> eff b
withOpsHandler handler comp1 = runDynamicEff comp2 handler
 where
  ops :: Operation ops (DynamicEff ops eff)
  ops = dynamicOps

  comp2 :: DynamicEff ops eff a
  comp2 = bindConstraint ops comp1

withDynamicHandler
  :: forall f ops handler eff a .
  ( Effect eff
  , EffOps ops
  , EffOps handler
  , DynamicOps handler
  , OpsConstraint ops eff
  )
  => DynamicHandler f ops handler eff
  -> (( OpsConstraint handler (DynamicEff handler eff)
      , OpsConstraint ops (DynamicEff handler eff)
      )
      => DynamicEff handler eff a)
  -> eff (f a)
withDynamicHandler (DynamicHandler handler1) comp1
  = comp2
   where
    comp2 :: eff (f a)
    comp2 = runDynamicEff comp3 handler2

    liftedOps :: Operation ops (DynamicEff handler eff)
    liftedOps = effmap liftDynamicEff captureOps

    context :: DynamicContext f ops handler eff
    context = runComp handler1 id captureOps

    handler2 :: OpsHandler handler a (f a) eff
    (DynamicContext handler2 _) = context

    handler3 :: Operation handler (DynamicEff handler eff)
    handler3 = dynamicOps

    comp3 :: DynamicEff handler eff a
    comp3 = bindConstraint handler3 $
      bindConstraint liftedOps comp1

