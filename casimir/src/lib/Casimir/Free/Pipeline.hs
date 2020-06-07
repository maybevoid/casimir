
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
  :: forall free ops1 handler m1 a b .
  ( Monad m1
  , Effect ops1
  , Effect handler
  , FreeOps handler
  , FreeHandler free
  , EffFunctor Lift (Operation ops1)
  )
  => Computation Lift ops1 (CoOpHandler handler a b) m1
  -> Pipeline Lift ops1 handler (Return a) (Return b) m1 m1
coopHandlerToPipeline handler1 = Pipeline pipeline
 where
  pipeline
    :: forall ops2
     . ( Effect ops2
       , EffFunctor Lift (Operation ops2)
       )
    => Computation Lift (handler ∪ ops2) (Return a) m1
    -> Computation Lift (ops1 ∪ ops2) (Return b) m1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall m2
       . (Monad m2)
      => Lift m1 m2
      -> Operation (ops1 ∪ ops2) m2
      -> Return b m2
    comp2 lift12 (UnionOps ops1 ops2) = Return comp4
     where
      handler2 :: CoOpHandler handler a b m2
      handler2 = runComp handler1 lift12 ops1

      comp3 :: free handler m2 a
      comp3 = returnVal $ runComp comp1
        (joinLift lift12 freeLiftEff)
        (freeOps ∪ effmap (Lift liftFree) ops2)

      comp4 :: m2 b
      comp4 = handleFree handler2 comp3

{-# INLINE genericCoOpHandlerToPipeline #-}
genericCoOpHandlerToPipeline
  :: forall free ops1 handler m1 .
  ( Monad m1
  , Effect ops1
  , Effect handler
  , FreeOps handler
  , ImplicitOps ops1
  , ImplicitOps handler
  , FreeHandler free
  )
  => BaseComputation ops1 (GenericCoOpHandler handler) m1
  -> GenericPipeline Lift ops1 handler m1
genericCoOpHandlerToPipeline handler1
  = transformePipeline $ Computation handler2
 where
  handler2
    :: forall m2 .
    (Monad m2)
    => Lift m1 m2
    -> Operation ops1 m2
    -> TransformerHandler (free handler) handler m2
  handler2 lift12 ops1
    = TransformerHandler freeOps freeLiftEff (Lift unliftFree)
    where
      (GenericCoOpHandler handler3) = runComp handler1 lift12 ops1

      unliftFree
        :: forall a .
        free handler m2 a
        -> m2 a
      unliftFree = handleFree handler3

{-# INLINE contextualHandlerToPipeline #-}
contextualHandlerToPipeline
  :: forall free w ops1 handler m1 .
  ( Monad m1
  , Effect ops1
  , Effect handler
  , FreeOps handler
  , FreeHandler free
  , ImplicitOps ops1
  , ImplicitOps handler
  )
  => BaseComputation ops1 (ContextualHandler w handler) m1
  -> GenericPipeline Lift ops1 handler m1
contextualHandlerToPipeline handler1
  = transformePipeline $ Computation handler2
 where
  handler2
    :: forall m2 .
    (Monad m2)
    => Lift m1 m2
    -> Operation ops1 m2
    -> TransformerHandler (free handler) handler m2
  handler2 lift12 ops1
    = TransformerHandler freeOps freeLiftEff (Lift unliftFree)
   where
    (ContextualHandler handler3 extract) = runComp handler1 lift12 ops1

    unliftFree
      :: forall a .
      free handler m2 a
      -> m2 a
    unliftFree m = do
      wx <- handleFree handler3 m
      extract wx
