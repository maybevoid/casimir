
module Casimir.Free.Pipeline
( coopHandlerToPipeline
, genericCoOpHandlerToPipeline
, contextualHandlerToPipeline
)
where

import Casimir.Base
import Casimir.Computation

import Casimir.Free.FreeEff
import Casimir.Free.FreeOps

{-# INLINE coopHandlerToPipeline #-}
coopHandlerToPipeline
  :: forall free ops1 handler eff1 a b .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  , FreeOps handler
  , FreeHandler free
  , EffFunctor Lift (Operation ops1)
  )
  => Computation Lift ops1 (CoOpHandler handler a b) eff1
  -> Pipeline Lift ops1 handler (Return a) (Return b) eff1 eff1
coopHandlerToPipeline handler1 = Pipeline pipeline
 where
  pipeline
    :: forall ops2
     . ( EffOps ops2
       , EffFunctor Lift (Operation ops2)
       )
    => Computation Lift (handler ∪ ops2) (Return a) eff1
    -> Computation Lift (ops1 ∪ ops2) (Return b) eff1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall eff2
       . (Effect eff2)
      => Lift eff1 eff2
      -> Operation (ops1 ∪ ops2) eff2
      -> Return b eff2
    comp2 lift12 (UnionOps ops1 ops2) = Return comp4
     where
      handler2 :: CoOpHandler handler a b eff2
      handler2 = runComp handler1 lift12 ops1

      comp3 :: free handler eff2 a
      comp3 = returnVal $ runComp comp1
        (joinLift lift12 freeLiftEff)
        (freeOps ∪ effmap (Lift liftFree) ops2)

      comp4 :: eff2 b
      comp4 = handleFree handler2 comp3

{-# INLINE genericCoOpHandlerToPipeline #-}
genericCoOpHandlerToPipeline
  :: forall free ops1 handler eff1 .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  , FreeOps handler
  , ImplicitOps ops1
  , ImplicitOps handler
  , FreeHandler free
  )
  => BaseComputation ops1 (GenericCoOpHandler handler) eff1
  -> GenericPipeline Lift ops1 handler eff1
genericCoOpHandlerToPipeline handler1
  = transformePipeline $ Computation handler2
 where
  handler2
    :: forall eff2 .
    (Effect eff2)
    => Lift eff1 eff2
    -> Operation ops1 eff2
    -> TransformerHandler (free handler) handler eff2
  handler2 lift12 ops1
    = TransformerHandler freeOps freeLiftEff (Lift unliftFree)
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
  , EffOps ops1
  , EffOps handler
  , FreeOps handler
  , FreeHandler free
  , ImplicitOps ops1
  , ImplicitOps handler
  )
  => BaseComputation ops1 (ContextualHandler w handler) eff1
  -> GenericPipeline Lift ops1 handler eff1
contextualHandlerToPipeline handler1
  = transformePipeline $ Computation handler2
 where
  handler2
    :: forall eff2 .
    (Effect eff2)
    => Lift eff1 eff2
    -> Operation ops1 eff2
    -> TransformerHandler (free handler) handler eff2
  handler2 lift12 ops1
    = TransformerHandler freeOps freeLiftEff (Lift unliftFree)
   where
    (ContextualHandler handler3 extract) = runComp handler1 lift12 ops1

    unliftFree
      :: forall a .
      free handler eff2 a
      -> eff2 a
    unliftFree eff = do
      wx <- handleFree handler3 eff
      extract wx
