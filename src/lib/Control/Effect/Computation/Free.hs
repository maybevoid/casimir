
module Control.Effect.Computation.Free
where

import Control.Monad.Trans.Free

import Control.Effect.Base
import Control.Effect.Computation.Class
import Control.Effect.Computation.Value
import Control.Effect.Computation.Pipeline

handleFree
  :: forall ops eff a r
   . (Effect eff, EffOps ops)
  => OpsHandler ops a r eff
  -> FreeEff ops eff a
  -> eff r
handleFree handler = handleFree'
 where
  handleFree'
   :: FreeEff ops eff a
    -> eff r
  handleFree' comp = runFreeT comp >>= handleComp

  handleComp
    :: FreeF (CoOperation ops) a (FreeEff ops eff a)
    -> eff r
  handleComp (Pure x) = handleReturn handler x
  handleComp (Free ops) = handleOps handler $ fmap handleFree' ops

withFreeOpsHandler
  :: forall ops eff a r
   . (Effect eff, EffOps ops)
  => OpsHandler ops a r eff
  -> ((OpsConstraint ops (FreeT (CoOperation ops) eff))
      => FreeEff ops eff a)
  -> eff r
withFreeOpsHandler handler comp1 = handleFree handler comp2
 where
  comp2 :: FreeEff ops eff a
  comp2 = bindConstraint ops comp1

  ops :: Operation ops (FreeT (CoOperation ops) eff)
  ops = freeOps

freeOpsHandlerToPipeline
  :: forall ops1 handler eff1 a b .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  )
  => Computation ops1 (OpsHandler handler a b) eff1
  -> Pipeline ops1 handler eff1 eff1 (Return a) (Return b)
freeOpsHandlerToPipeline handler1 = Pipeline pipeline
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

      comp3 :: FreeEff handler eff2 a
      comp3 = returnVal $ runComp comp1
        (liftFree . lift12)
        (UnionOps (freeOps) (effmap liftFree ops2))

      comp4 :: eff2 b
      comp4 = handleFree handler2 comp3

freeGenericOpsHandlerToPipeline
  :: forall ops1 handler eff1 .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  )
  => Computation ops1 (GenericOpsHandler handler) eff1
  -> GenericPipeline ops1 handler eff1
freeGenericOpsHandlerToPipeline handler1
  = transformerPipeline $ Computation handler2
 where
  handler2
    :: forall eff2 .
    (Effect eff2)
    => LiftEff eff1 eff2
    -> Operation ops1 eff2
    -> TransformerHandler (FreeT (CoOperation handler)) handler eff2
  handler2 lift12 ops1
    = TransformerHandler (freeOps) liftFree unliftFree
    where
      (GenericOpsHandler handler3) = runComp handler1 lift12 ops1

      unliftFree
        :: forall a .
        FreeEff handler eff2 a
        -> eff2 a
      unliftFree = handleFree handler3

freeContextualHandlerToPipeline
  :: forall w ops1 handler eff1 .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  )
  => Computation ops1 (ContextualHandler w handler) eff1
  -> GenericPipeline ops1 handler eff1
freeContextualHandlerToPipeline handler1
  = transformerPipeline $ Computation handler2
 where
  handler2
    :: forall eff2 .
    (Effect eff2)
    => LiftEff eff1 eff2
    -> Operation ops1 eff2
    -> TransformerHandler (FreeT (CoOperation handler)) handler eff2
  handler2 lift12 ops1
    = TransformerHandler (freeOps) liftFree unliftFree
   where
    (ContextualHandler handler3 extract) = runComp handler1 lift12 ops1

    unliftFree
      :: forall a .
      FreeEff handler eff2 a
      -> eff2 a
    unliftFree eff = do
      wx <- handleFree handler3 eff
      extract wx
