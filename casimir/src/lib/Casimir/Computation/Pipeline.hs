
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

newtype Pipeline lift ops1 handler comp1 comp2 eff1 eff2
  = Pipeline
  { runExactPipeline
      :: forall ops2
       . ( ImplicitOps ops2
         , EffFunctor lift (Operation ops2)
         )
      => Computation lift (handler ∪ ops2) comp1 eff1
      -> Computation lift (ops1 ∪ ops2) comp2 eff2
  }

type BasePipeline = Pipeline Lift

data TransformerHandler t handler eff = TransformerHandler {
  tCoOpHandler :: Operation handler (t eff),
  tLift :: Lift eff (t eff),
  tUnliftEff :: Lift (t eff) eff
}

type SimplePipeline lift ops handler comp eff
  = Pipeline lift ops handler comp comp eff eff

type GenericPipeline lift ops handler eff
  = forall comp
     . (EffFunctor lift comp)
    => SimplePipeline lift ops handler comp eff

opsHandlerToPipeline
  :: forall ops1 lift handler eff
   . ( Monad eff
     , ImplicitOps ops1
     , ImplicitOps handler
     , LiftMonoid lift
      , EffFunctor lift (Operation handler)
     )
  => OpsHandler lift ops1 handler eff
  -> (forall comp
       . (EffFunctor lift comp)
      => SimplePipeline lift ops1 handler comp eff )
opsHandlerToPipeline handler1 = Pipeline pipeline
 where
  pipeline :: forall ops2 comp
     . ( ImplicitOps ops2
       , EffFunctor lift comp
       )
    => Computation lift (handler ∪ ops2) comp eff
    -> Computation lift (ops1 ∪ ops2) comp eff
  pipeline comp1 = bindOpsHandlerWithCast @(ops1 ∪ ops2)
    cast cast
    handler1 comp1

{-# INLINE transformePipeline #-}
transformePipeline
  :: forall t ops1 handler eff1 .
  ( Monad eff1
  , ImplicitOps ops1
  , ImplicitOps handler
  , (forall eff . (Monad eff) => Monad (t eff))
  )
  => Computation Lift ops1 (TransformerHandler t handler) eff1
  -> GenericPipeline Lift ops1 handler eff1
transformePipeline handler1 = Pipeline pipeline
 where
  {-# INLINE pipeline #-}
  pipeline :: forall ops2 comp .
    ( ImplicitOps ops2
    , EffFunctor Lift comp
    , EffFunctor Lift (Operation ops2)
    )
    => BaseComputation (handler ∪ ops2) comp eff1
    -> BaseComputation (ops1 ∪ ops2) comp eff1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall eff2 . (Monad eff2)
      => Lift eff1 eff2
      -> Operation (ops1 ∪ ops2) eff2
      -> comp eff2
    comp2 lift12 (UnionOps ops1 ops2) = effmap unliftT comp3
     where
      TransformerHandler coopHandler liftT unliftT
        = runComp handler1 lift12 ops1

      comp3 :: comp (t eff2)
      comp3 = runComp comp1 (joinLift lift12 liftT) $
        UnionOps coopHandler $ effmap liftT ops2

castPipelineOps
  :: forall ops1 ops2 lift handler comp1 comp2 eff1 eff2  .
  ( Monad eff1
  , Monad eff2
  , ImplicitOps ops1
  , ImplicitOps ops2
  , ImplicitOps handler
  )
  => OpsCast ops2 ops1
  -> Pipeline lift ops1 handler comp1 comp2 eff1 eff2
  -> Pipeline lift ops2 handler comp1 comp2 eff1 eff2
castPipelineOps cast21 pipeline1 = Pipeline pipeline2
 where
  pipeline2 :: forall ops3
     . ( ImplicitOps ops3
       , EffFunctor lift (Operation ops3)
       )
    => Computation lift (handler ∪ ops3) comp1 eff1
    -> Computation lift (ops2 ∪ ops3) comp2 eff2
  pipeline2 comp1 = comp2
   where
    comp2 :: Computation lift (ops2 ∪ ops3) comp2 eff2
    comp2 = castComputation cast21' comp3

    comp3 :: Computation lift (ops1 ∪ ops3) comp2 eff2
    comp3 = runExactPipeline pipeline1 comp1

    cast21' :: OpsCast (ops2 ∪ ops3) (ops1 ∪ ops3)
    cast21' = extendCast @ops2 @ops1 cast21

castPipelineHandler
  :: forall ops1 lift handler1 handler2 comp1 comp2 eff1 eff2
   . ( Monad eff1
     , Monad eff2
     , ImplicitOps ops1
     , ImplicitOps handler1
     , ImplicitOps handler2
     , LiftMonoid lift
     , EffFunctor lift (Operation handler1)
     )
  => OpsCast handler1 handler2
  -> Pipeline lift ops1 handler1 comp1 comp2 eff1 eff2
  -> Pipeline lift ops1 handler2 comp1 comp2 eff1 eff2
castPipelineHandler cast1 pipeline1 = Pipeline pipeline2
 where
  pipeline2
    :: forall ops2
     . ( ImplicitOps ops2
       , EffFunctor lift (Operation ops2)
       )
    => Computation lift (handler2 ∪ ops2) comp1 eff1
    -> Computation lift (ops1 ∪ ops2) comp2 eff2
  pipeline2 comp1 =
    runExactPipeline pipeline1 comp2
   where
    comp2 :: Computation lift (handler1 ∪ ops2) comp1 eff1
    comp2 = castComputation cast2 comp1

    cast2 :: OpsCast (handler1 ∪ ops2) (handler2 ∪ ops2)
    cast2 = extendCast @handler1 @handler2 cast1

composeExactPipelines
  :: forall ops1 ops2 lift handler1 handler2 comp1 comp2 comp3 eff1 eff2 eff3 .
  ( Monad eff1
  , Monad eff2
  , Monad eff3
  , LiftMonoid lift
  , ImplicitOps ops1
  , ImplicitOps ops2
  , ImplicitOps handler1
  , ImplicitOps handler2
  , EffFunctor lift (Operation ops1)
  , EffFunctor lift (Operation handler1)
  , EffFunctor lift (Operation handler2)
  )
  => Pipeline lift (handler2 ∪ ops1) handler1 comp1 comp2 eff1 eff2
  -> Pipeline lift ops2 handler2 comp2 comp3 eff2 eff3
  -> Pipeline lift (ops1 ∪ ops2) (handler1 ∪ handler2) comp1 comp3 eff1 eff3
composeExactPipelines pipeline1 pipeline2 = Pipeline pipeline3
 where
  pipeline3 :: forall ops3
     . ( ImplicitOps ops3
       , EffFunctor lift (Operation ops3)
       , EffFunctor lift (UnionOps (Operation handler2) (Operation ops3))
       )
    => Computation lift ((handler1 ∪ handler2) ∪ ops3) comp1 eff1
    -> Computation lift ((ops1 ∪ ops2) ∪ ops3) comp3 eff3
  pipeline3 comp1 =
    castComputation cast comp4
     where
      comp1' :: Computation lift (handler1 ∪ handler2 ∪ ops3) comp1 eff1
      comp1' = castComputation cast comp1

      comp3 :: Computation lift ((handler2 ∪ ops1) ∪ (handler2 ∪ ops3)) comp2 eff2
      comp3 = runExactPipeline pipeline1 comp1'

      comp3' :: Computation lift (handler2 ∪ ops1 ∪ ops3) comp2 eff2
      comp3' = castComputation cast comp3

      comp4 :: Computation lift (ops2 ∪ ops1 ∪ ops3) comp3 eff3
      comp4 = runExactPipeline pipeline2 comp3'

runPipelineWithCast
  :: forall ops3 ops1 ops2 lift handler comp1 comp2 eff1 eff2 .
  ( Monad eff1
  , Monad eff2
  , ImplicitOps ops1
  , ImplicitOps ops2
  , ImplicitOps ops3
  , ImplicitOps handler
  , EffFunctor lift (Operation ops3)
  )
  => OpsCast ops3 ops1
  -> OpsCast (handler ∪ ops3) ops2
  -> Pipeline lift ops1 handler comp1 comp2 eff1 eff2
  -> Computation lift ops2 comp1 eff1
  -> Computation lift ops3 comp2 eff2
runPipelineWithCast cast1 cast2 pipeline1 comp1 =
  castComputation cast $
    runExactPipeline pipeline2 comp2
 where
   pipeline2 :: Pipeline lift ops3 handler comp1 comp2 eff1 eff2
   pipeline2 = castPipelineOps cast1 pipeline1

   comp2 :: Computation lift (handler ∪ ops3) comp1 eff1
   comp2 = castComputation cast2 comp1

runPipeline
  :: forall ops3 ops1 ops2 lift handler comp1 comp2 eff1 eff2 .
  ( Monad eff1
  , Monad eff2
  , ops3 ⊇ ops1
  , (handler ∪ ops3) ⊇ ops2
  , ImplicitOps ops1
  , ImplicitOps ops2
  , ImplicitOps ops3
  , ImplicitOps handler
  , EffFunctor lift (Operation ops3)
  )
  => Pipeline lift ops1 handler comp1 comp2 eff1 eff2
  -> Computation lift ops2 comp1 eff1
  -> Computation lift ops3 comp2 eff2
runPipeline = runPipelineWithCast
  (entailOps @ops3 @ops1)
  (entailOps @(handler ∪ ops3) @ops2)

composePipelinesWithCast
  :: forall ops1 ops2 ops3 lift handler1 handler2 handler3
      comp1 comp2 comp3 eff1 eff2 eff3
   . ( Monad eff1
     , Monad eff2
     , Monad eff3
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
  -> Pipeline lift ops1 handler1 comp1 comp2 eff1 eff2
  -> Pipeline lift ops2 handler2 comp2 comp3 eff2 eff3
  -> Pipeline lift ops3 handler3 comp1 comp3 eff1 eff3
composePipelinesWithCast cast1 cast2 cast3 pipeline1 pipeline2
  = castPipelineHandler cast3 $
    castPipelineOps cast $
    composeExactPipelines pipeline1' pipeline2'
  where
    pipeline1' :: Pipeline lift (handler2 ∪ ops3) handler1 comp1 comp2 eff1 eff2
    pipeline1' = castPipelineOps cast1 pipeline1

    pipeline2' :: Pipeline lift ops3 handler2 comp2 comp3 eff2 eff3
    pipeline2' = castPipelineOps cast2 pipeline2

composePipelines
  :: forall ops1 ops2 ops3 lift handler1 handler2 handler3 comp1 comp2 comp3 eff1 eff2 eff3
   . ( Monad eff1
     , Monad eff2
     , Monad eff3
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
  => Pipeline lift ops1 handler1 comp1 comp2 eff1 eff2
  -> Pipeline lift ops2 handler2 comp2 comp3 eff2 eff3
  -> Pipeline lift ops3 handler3 comp1 comp3 eff1 eff3
composePipelines = composePipelinesWithCast
  (entailOps @(handler2 ∪ ops3) @ops1)
  (entailOps @ops3 @ops2)
  (entailOps @(handler1 ∪ handler2) @handler3)
