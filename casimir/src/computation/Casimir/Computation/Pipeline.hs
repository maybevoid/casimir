
module Casimir.Computation.Pipeline
  ( Pipeline (..)
  , BasePipeline
  , TransformerHandler (..)
  , SimplePipeline
  , GenericPipeline
  , opsHandlerToPipeline
  , transformePipeline
  , castPipelineOps
  , castPipelineHandler
  , composePipelines
  , composeExactPipelines
  , runPipeline
  , runPipelineWithCast
  , composePipelinesWithCast
  )
where

import Casimir.Base
import Casimir.Cast
import Casimir.Computation.Cast
import Casimir.Computation.Handler
import Casimir.Computation.Computation

newtype Pipeline lift ops1 handler comp1 comp2 m1 m2
  = Pipeline
  { runExactPipeline
      :: forall ops2
       . ( ImplicitOps ops2
         , EffFunctor lift (Operation ops2)
         )
      => Computation lift (handler ∪ ops2) comp1 m1
      -> Computation lift (ops1 ∪ ops2) comp2 m2
  }

type BasePipeline = Pipeline Lift

data TransformerHandler t handler m = TransformerHandler {
  tCoOpHandler :: Operation handler (t m),
  tLift :: Lift m (t m),
  tUnliftEff :: Lift (t m) m
}

type SimplePipeline lift ops handler comp m
  = Pipeline lift ops handler comp comp m m

type GenericPipeline lift ops handler m
  = forall comp
     . (EffFunctor lift comp)
    => SimplePipeline lift ops handler comp m

opsHandlerToPipeline
  :: forall ops1 lift handler m
   . ( Monad m
     , ImplicitOps ops1
     , ImplicitOps handler
     , LiftMonoid lift
      , EffFunctor lift (Operation handler)
     )
  => OpsHandler lift ops1 handler m
  -> (forall comp
       . (EffFunctor lift comp)
      => SimplePipeline lift ops1 handler comp m )
opsHandlerToPipeline handler1 = Pipeline pipeline
 where
  pipeline :: forall ops2 comp
     . ( ImplicitOps ops2
       , EffFunctor lift comp
       )
    => Computation lift (handler ∪ ops2) comp m
    -> Computation lift (ops1 ∪ ops2) comp m
  pipeline comp1 = bindOpsHandlerWithCast @(ops1 ∪ ops2)
    cast cast
    handler1 comp1

{-# INLINE transformePipeline #-}
transformePipeline
  :: forall t ops1 handler m1 .
  ( Monad m1
  , ImplicitOps ops1
  , ImplicitOps handler
  , (forall m . (Monad m) => Monad (t m))
  )
  => Computation Lift ops1 (TransformerHandler t handler) m1
  -> GenericPipeline Lift ops1 handler m1
transformePipeline handler1 = Pipeline pipeline
 where
  {-# INLINE pipeline #-}
  pipeline :: forall ops2 comp .
    ( ImplicitOps ops2
    , EffFunctor Lift comp
    , EffFunctor Lift (Operation ops2)
    )
    => BaseComputation (handler ∪ ops2) comp m1
    -> BaseComputation (ops1 ∪ ops2) comp m1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall m2 . (Monad m2)
      => Lift m1 m2
      -> Operation (ops1 ∪ ops2) m2
      -> comp m2
    comp2 lift12 (UnionOps ops1 ops2) = effmap unliftT comp3
     where
      TransformerHandler coopHandler liftT unliftT
        = runComp handler1 lift12 ops1

      comp3 :: comp (t m2)
      comp3 = runComp comp1 (joinLift lift12 liftT) $
        UnionOps coopHandler $ effmap liftT ops2

castPipelineOps
  :: forall ops1 ops2 lift handler comp1 comp2 m1 m2  .
  ( Monad m1
  , Monad m2
  , ImplicitOps ops1
  , ImplicitOps ops2
  , ImplicitOps handler
  )
  => OpsCast ops2 ops1
  -> Pipeline lift ops1 handler comp1 comp2 m1 m2
  -> Pipeline lift ops2 handler comp1 comp2 m1 m2
castPipelineOps cast21 pipeline1 = Pipeline pipeline2
 where
  pipeline2 :: forall ops3
     . ( ImplicitOps ops3
       , EffFunctor lift (Operation ops3)
       )
    => Computation lift (handler ∪ ops3) comp1 m1
    -> Computation lift (ops2 ∪ ops3) comp2 m2
  pipeline2 comp1 = comp2
   where
    comp2 :: Computation lift (ops2 ∪ ops3) comp2 m2
    comp2 = castComputation cast21' comp3

    comp3 :: Computation lift (ops1 ∪ ops3) comp2 m2
    comp3 = runExactPipeline pipeline1 comp1

    cast21' :: OpsCast (ops2 ∪ ops3) (ops1 ∪ ops3)
    cast21' = extendCast @ops2 @ops1 cast21

castPipelineHandler
  :: forall ops1 lift handler1 handler2 comp1 comp2 m1 m2
   . ( Monad m1
     , Monad m2
     , ImplicitOps ops1
     , ImplicitOps handler1
     , ImplicitOps handler2
     , LiftMonoid lift
     , EffFunctor lift (Operation handler1)
     )
  => OpsCast handler1 handler2
  -> Pipeline lift ops1 handler1 comp1 comp2 m1 m2
  -> Pipeline lift ops1 handler2 comp1 comp2 m1 m2
castPipelineHandler cast1 pipeline1 = Pipeline pipeline2
 where
  pipeline2
    :: forall ops2
     . ( ImplicitOps ops2
       , EffFunctor lift (Operation ops2)
       )
    => Computation lift (handler2 ∪ ops2) comp1 m1
    -> Computation lift (ops1 ∪ ops2) comp2 m2
  pipeline2 comp1 =
    runExactPipeline pipeline1 comp2
   where
    comp2 :: Computation lift (handler1 ∪ ops2) comp1 m1
    comp2 = castComputation cast2 comp1

    cast2 :: OpsCast (handler1 ∪ ops2) (handler2 ∪ ops2)
    cast2 = extendCast @handler1 @handler2 cast1

composeExactPipelines
  :: forall ops1 ops2 lift handler1 handler2 comp1 comp2 comp3 m1 m2 m3 .
  ( Monad m1
  , Monad m2
  , Monad m3
  , LiftMonoid lift
  , ImplicitOps ops1
  , ImplicitOps ops2
  , ImplicitOps handler1
  , ImplicitOps handler2
  , EffFunctor lift (Operation ops1)
  , EffFunctor lift (Operation handler1)
  , EffFunctor lift (Operation handler2)
  )
  => Pipeline lift (handler2 ∪ ops1) handler1 comp1 comp2 m1 m2
  -> Pipeline lift ops2 handler2 comp2 comp3 m2 m3
  -> Pipeline lift (ops1 ∪ ops2) (handler1 ∪ handler2) comp1 comp3 m1 m3
composeExactPipelines pipeline1 pipeline2 = Pipeline pipeline3
 where
  pipeline3 :: forall ops3
     . ( ImplicitOps ops3
       , EffFunctor lift (Operation ops3)
       , EffFunctor lift (UnionOps (Operation handler2) (Operation ops3))
       )
    => Computation lift ((handler1 ∪ handler2) ∪ ops3) comp1 m1
    -> Computation lift ((ops1 ∪ ops2) ∪ ops3) comp3 m3
  pipeline3 comp1 =
    castComputation cast comp4
     where
      comp1' :: Computation lift (handler1 ∪ handler2 ∪ ops3) comp1 m1
      comp1' = castComputation cast comp1

      comp3 :: Computation lift ((handler2 ∪ ops1) ∪ (handler2 ∪ ops3)) comp2 m2
      comp3 = runExactPipeline pipeline1 comp1'

      comp3' :: Computation lift (handler2 ∪ ops1 ∪ ops3) comp2 m2
      comp3' = castComputation cast comp3

      comp4 :: Computation lift (ops2 ∪ ops1 ∪ ops3) comp3 m3
      comp4 = runExactPipeline pipeline2 comp3'

runPipelineWithCast
  :: forall ops3 ops1 ops2 lift handler comp1 comp2 m1 m2 .
  ( Monad m1
  , Monad m2
  , ImplicitOps ops1
  , ImplicitOps ops2
  , ImplicitOps ops3
  , ImplicitOps handler
  , EffFunctor lift (Operation ops3)
  )
  => OpsCast ops3 ops1
  -> OpsCast (handler ∪ ops3) ops2
  -> Pipeline lift ops1 handler comp1 comp2 m1 m2
  -> Computation lift ops2 comp1 m1
  -> Computation lift ops3 comp2 m2
runPipelineWithCast cast1 cast2 pipeline1 comp1 =
  castComputation cast $
    runExactPipeline pipeline2 comp2
 where
   pipeline2 :: Pipeline lift ops3 handler comp1 comp2 m1 m2
   pipeline2 = castPipelineOps cast1 pipeline1

   comp2 :: Computation lift (handler ∪ ops3) comp1 m1
   comp2 = castComputation cast2 comp1

runPipeline
  :: forall ops3 ops1 ops2 lift handler comp1 comp2 m1 m2 .
  ( Monad m1
  , Monad m2
  , ops3 ⊇ ops1
  , (handler ∪ ops3) ⊇ ops2
  , ImplicitOps ops1
  , ImplicitOps ops2
  , ImplicitOps ops3
  , ImplicitOps handler
  , EffFunctor lift (Operation ops3)
  )
  => Pipeline lift ops1 handler comp1 comp2 m1 m2
  -> Computation lift ops2 comp1 m1
  -> Computation lift ops3 comp2 m2
runPipeline = runPipelineWithCast
  (entailOps @ops3 @ops1)
  (entailOps @(handler ∪ ops3) @ops2)

composePipelinesWithCast
  :: forall ops1 ops2 ops3 lift handler1 handler2 handler3
      comp1 comp2 comp3 m1 m2 m3
   . ( Monad m1
     , Monad m2
     , Monad m3
     , LiftMonoid lift
     , ImplicitOps ops1
     , ImplicitOps ops2
     , ImplicitOps ops3
     , ImplicitOps handler1
     , ImplicitOps handler2
     , ImplicitOps handler3
     , EffFunctor lift (Operation ops3)
     , EffFunctor lift (Operation handler1)
     , EffFunctor lift (Operation handler2)
     , EffFunctor lift (Operation (handler1 ∪ handler2))
     )
  => OpsCast (handler2 ∪ ops3) ops1
  -> OpsCast ops3 ops2
  -> OpsCast (handler1 ∪ handler2) handler3
  -> Pipeline lift ops1 handler1 comp1 comp2 m1 m2
  -> Pipeline lift ops2 handler2 comp2 comp3 m2 m3
  -> Pipeline lift ops3 handler3 comp1 comp3 m1 m3
composePipelinesWithCast cast1 cast2 cast3 pipeline1 pipeline2
  = castPipelineHandler cast3 $
    castPipelineOps cast $
    composeExactPipelines pipeline1' pipeline2'
  where
    pipeline1' :: Pipeline lift (handler2 ∪ ops3) handler1 comp1 comp2 m1 m2
    pipeline1' = castPipelineOps cast1 pipeline1

    pipeline2' :: Pipeline lift ops3 handler2 comp2 comp3 m2 m3
    pipeline2' = castPipelineOps cast2 pipeline2

composePipelines
  :: forall ops1 ops2 ops3 lift handler1 handler2 handler3 comp1 comp2 comp3 m1 m2 m3
   . ( Monad m1
     , Monad m2
     , Monad m3
     , LiftMonoid lift
     , ImplicitOps ops1
     , ImplicitOps ops2
     , ImplicitOps ops3
     , ImplicitOps handler1
     , ImplicitOps handler2
     , ImplicitOps handler3
     , (handler2 ∪ ops3) ⊇ ops1
     , ops3 ⊇ ops2
     , (handler1 ∪ handler2) ⊇ handler3
     , EffFunctor lift (Operation ops3)
     , EffFunctor lift (Operation handler1)
     , EffFunctor lift (Operation handler2)
     , EffFunctor lift (Operation (handler1 ∪ handler2))
     )
  => Pipeline lift ops1 handler1 comp1 comp2 m1 m2
  -> Pipeline lift ops2 handler2 comp2 comp3 m2 m3
  -> Pipeline lift ops3 handler3 comp1 comp3 m1 m3
composePipelines = composePipelinesWithCast
  (entailOps @(handler2 ∪ ops3) @ops1)
  (entailOps @ops3 @ops2)
  (entailOps @(handler1 ∪ handler2) @handler3)
