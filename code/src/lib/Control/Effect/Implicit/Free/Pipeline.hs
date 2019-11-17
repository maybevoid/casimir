
module Control.Effect.Implicit.Free.Pipeline
( coopHandlerToPipeline
, genericCoOpHandlerToPipeline
, contextualHandlerToPipeline
)
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation

import Control.Effect.Implicit.Free.FreeEff
import Control.Effect.Implicit.Free.FreeOps

{-# INLINE coopHandlerToPipeline #-}
coopHandlerToPipeline
  :: forall free ops1 handler eff1 a b .
  ( Effect eff1
  , BaseOps ops1
  , FreeOps handler
  , BaseOps handler
  , FreeHandler free
  )
  => BaseComputation ops1 (CoOpHandler handler a b) eff1
  -> BasePipeline ops1 handler (Return a) (Return b) eff1 eff1
coopHandlerToPipeline handler1 = Pipeline pipeline
 where
  pipeline
    :: forall ops2 .
    (BaseOps ops2)
    => BaseComputation (handler ∪ ops2) (Return a) eff1
    -> BaseComputation (ops1 ∪ ops2) (Return b) eff1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall eff2 .
      (Effect eff2)
      => LiftEff eff1 eff2
      -> Operation (ops1 ∪ ops2) eff2
      -> Return b eff2
    comp2 lift12 (UnionOps ops1 ops2) = Return comp4
     where
      handler2 :: CoOpHandler handler a b eff2
      handler2 = runComp handler1 lift12 ops1

      comp3 :: free handler eff2 a
      comp3 = returnVal $ runComp comp1
        (joinLiftEff lift12 freeLiftEff)
        (freeOps ∪ effmap liftFree ops2)

      comp4 :: eff2 b
      comp4 = handleFree handler2 comp3

{-# INLINE genericCoOpHandlerToPipeline #-}
genericCoOpHandlerToPipeline
  :: forall free ops1 handler eff1 .
  ( Effect eff1
  , BaseOps ops1
  , FreeOps handler
  , BaseOps handler
  , FreeHandler free
  )
  => BaseComputation ops1 (GenericCoOpHandler handler) eff1
  -> GenericPipeline LiftEff ops1 handler eff1
genericCoOpHandlerToPipeline handler1
  = transformePipeline $ Computation handler2
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
      (GenericCoOpHandler handler3) = runComp handler1 lift12 ops1

      unliftFree
        :: forall a .
        free handler eff2 a
        -> eff2 a
      unliftFree = handleFree handler3

{-# INLINE contextualHandlerToPipeline #-}
contextualHandlerToPipeline
  :: forall free w ops1 handler eff1 .
  ( Effect eff1
  , BaseOps ops1
  , FreeOps handler
  , BaseOps handler
  , FreeHandler free
  )
  => BaseComputation ops1 (ContextualHandler w handler) eff1
  -> GenericPipeline LiftEff ops1 handler eff1
contextualHandlerToPipeline handler1
  = transformePipeline $ Computation handler2
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
