
module Control.Effect.Dynamic.Pipeline
where

import Control.Effect.Base
import Control.Effect.Computation
import Control.Effect.Dynamic.Lift
import Control.Effect.Dynamic.Class

opsHandlerToPipeline
  :: forall ops1 handler eff1 a b .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  , DynamicOps handler
  )
  => Computation ops1 (OpsHandler handler a b) eff1
  -> Pipeline ops1 handler eff1 eff1 (Return a) (Return b)
opsHandlerToPipeline handler1 = Pipeline pipeline
 where
  pipeline
    :: forall ops2 .
    (EffOps ops2)
    => Computation (Union handler ops2) (Return a) eff1
    -> Computation (Union ops1 ops2) (Return b) eff1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall eff2 .
      (Effect eff2)
      => LiftEff eff1 eff2
      -> Operation (Union ops1 ops2) eff2
      -> Return b eff2
    comp2 lift12 (UnionOps ops1 ops2) = Return comp4
     where
      handler2 :: OpsHandler handler a b eff2
      handler2 = runComp handler1 lift12 ops1

      comp3 :: DynamicEff handler eff2 a
      comp3 = returnVal $ runComp comp1
        (liftDynamicEff . lift12)
        (UnionOps dynamicOps (effmap liftDynamicEff ops2))

      comp4 :: eff2 b
      comp4 = runDynamicEff comp3 handler2

genericOpsHandlerToPipeline
  :: forall ops1 handler eff1 .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  , DynamicOps handler
  )
  => Computation ops1 (GenericOpsHandler handler) eff1
  -> GenericPipeline ops1 handler eff1
genericOpsHandlerToPipeline handler1
  = transformerPipeline $ Computation handler2
 where
  handler2
    :: forall eff2 .
    (Effect eff2)
    => LiftEff eff1 eff2
    -> Operation ops1 eff2
    -> TransformerHandler (DynamicEff handler) handler eff2
  handler2 lift12 ops1
    = TransformerHandler dynamicOps liftDynamicEff unliftDynamic
    where
      (GenericOpsHandler handler3) = runComp handler1 lift12 ops1

      unliftDynamic
        :: forall a .
        DynamicEff handler eff2 a
        -> eff2 a
      unliftDynamic eff = do
        runDynamicEff eff handler3

contextualHandlerToPipeline
  :: forall w ops1 handler eff1 .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  , DynamicOps handler
  )
  => Computation ops1 (ContextualHandler w handler) eff1
  -> GenericPipeline ops1 handler eff1
contextualHandlerToPipeline handler1
  = transformerPipeline $ Computation handler2
 where
  handler2
    :: forall eff2 .
    (Effect eff2)
    => LiftEff eff1 eff2
    -> Operation ops1 eff2
    -> TransformerHandler (DynamicEff handler) handler eff2
  handler2 lift12 ops1
    = TransformerHandler dynamicOps liftDynamicEff unliftDynamic
   where
    (ContextualHandler handler3 extract) = runComp handler1 lift12 ops1

    unliftDynamic
      :: forall a .
      DynamicEff handler eff2 a
      -> eff2 a
    unliftDynamic eff = do
      wx <- runDynamicEff eff handler3
      extract wx
