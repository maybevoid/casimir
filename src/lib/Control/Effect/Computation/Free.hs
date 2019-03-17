{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Effect.Computation.Free
where

import Control.Effect.Base
import Control.Effect.Computation.Class
import Control.Effect.Computation.Value
import Control.Effect.Computation.Pipeline

withOpsHandler
  :: forall free ops eff a r
   . ( Effect eff
     , EffOps ops
     , FreeEff free
     )
  => OpsHandler ops a r eff
  -> ((OpsConstraint ops (free ops eff))
      => free ops eff a)
  -> eff r
withOpsHandler handler comp1 = handleFree handler comp2
 where
  comp2 :: free ops eff a
  comp2 = bindConstraint ops comp1

  ops :: Operation ops (free ops eff)
  ops = freeOps

opsHandlerToPipeline
  :: forall free ops1 handler eff1 a b .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  , FreeEff free
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

      comp3 :: free handler eff2 a
      comp3 = returnVal $ runComp comp1
        (joinLift lift12 freeLiftEff)
        (UnionOps freeOps (effmap liftFree ops2))

      comp4 :: eff2 b
      comp4 = handleFree handler2 comp3

genericOpsHandlerToPipeline
  :: forall free ops1 handler eff1 .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  , FreeEff free
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
    -> TransformerHandler (free handler) handler eff2
  handler2 lift12 ops1
    = TransformerHandler freeOps freeLiftEff (mkLiftEff unliftFree)
    where
      (GenericOpsHandler handler3) = runComp handler1 lift12 ops1

      unliftFree
        :: forall a .
        free handler eff2 a
        -> eff2 a
      unliftFree = handleFree handler3

contextualHandlerToPipeline
  :: forall free w ops1 handler eff1 .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  , FreeEff free
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
    -> TransformerHandler (free handler) handler eff2
  handler2 lift12 ops1
    = TransformerHandler freeOps freeLiftEff (mkLiftEff unliftFree)
   where
    (ContextualHandler handler3 extract) = runComp handler1 lift12 ops1

    unliftFree
      :: forall a .
      free handler eff2 a
      -> eff2 a
    unliftFree eff = do
      wx <- handleFree handler3 eff
      extract wx
