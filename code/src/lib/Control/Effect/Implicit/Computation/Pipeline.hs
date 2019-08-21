
module Control.Effect.Implicit.Computation.Pipeline
  ( Pipeline (..)
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

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Cast
import Control.Effect.Implicit.Computation.Cast
import Control.Effect.Implicit.Computation.Handler
import Control.Effect.Implicit.Computation.Computation

newtype Pipeline ops1 handler comp1 comp2 eff1 eff2
  = Pipeline {
      runExactPipeline
        :: forall ops2 .
        (BaseOps ops2)
        => Computation (handler ∪ ops2) comp1 eff1
        -> Computation (ops1 ∪ ops2) comp2 eff2
  }

data TransformerHandler t handler eff = TransformerHandler {
  tCoOpHandler :: handler (t eff),
  tLiftEff :: LiftEff eff (t eff),
  tUnliftEff :: LiftEff (t eff) eff
}

type SimplePipeline ops handler comp eff
  = Pipeline ops handler comp comp eff eff

type GenericPipeline ops handler eff
  = forall comp .
    (EffFunctor comp)
    => SimplePipeline ops handler comp eff

opsHandlerToPipeline
  :: forall ops1 handler eff .
  ( Effect eff
  , BaseOps ops1
  , BaseOps handler
  )
  => OpsHandler ops1 handler eff
  -> (forall comp .
      (EffFunctor comp)
      => SimplePipeline ops1 handler comp eff )
opsHandlerToPipeline handler1 = Pipeline pipeline
 where
  pipeline :: forall ops2 comp .
    (BaseOps ops2, EffFunctor comp)
    => Computation (handler ∪ ops2) comp eff
    -> Computation (ops1 ∪ ops2) comp eff
  pipeline comp1 = bindOpsHandlerWithCast @(ops1 ∪ ops2)
    cast cast
    handler1 comp1

{-# INLINE transformePipeline #-}
transformePipeline
  :: forall t ops1 handler eff1 .
  ( Effect eff1
  , BaseOps ops1
  , BaseOps handler
  , (forall eff . (Effect eff) => Effect (t eff))
  )
  => Computation ops1 (TransformerHandler t handler) eff1
  -> GenericPipeline ops1 handler eff1
transformePipeline handler1 = Pipeline pipeline
 where
  {-# INLINE pipeline #-}
  pipeline :: forall ops2 comp .
    ( BaseOps ops2
    , EffFunctor ops2
    , EffFunctor comp
    )
    => Computation (handler ∪ ops2) comp eff1
    -> Computation (ops1 ∪ ops2) comp eff1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall eff2 . (Effect eff2)
      => LiftEff eff1 eff2
      -> (ops1 ∪ ops2) eff2
      -> comp eff2
    comp2 lift12 (Union ops1 ops2) = applyEffmap unliftT comp3
     where
      TransformerHandler coopHandler liftT unliftT
        = runComp handler1 lift12 ops1

      comp3 :: comp (t eff2)
      comp3 = runComp comp1 (joinLift lift12 liftT) $
        Union coopHandler $ applyEffmap liftT ops2

castPipelineOps
  :: forall ops1 ops2 handler comp1 comp2 eff1 eff2  .
  ( Effect eff1
  , Effect eff2
  , BaseOps ops1
  , BaseOps ops2
  , BaseOps handler
  )
  => OpsCast ops2 ops1
  -> Pipeline ops1 handler comp1 comp2 eff1 eff2
  -> Pipeline ops2 handler comp1 comp2 eff1 eff2
castPipelineOps cast21 pipeline1 = Pipeline pipeline2
 where
  pipeline2 :: forall ops3 .
    (BaseOps ops3)
    => Computation (handler ∪ ops3) comp1 eff1
    -> Computation (ops2 ∪ ops3) comp2 eff2
  pipeline2 comp1 = comp2
   where
    comp2 :: Computation (ops2 ∪ ops3) comp2 eff2
    comp2 = castComputation cast21' comp3

    comp3 :: Computation (ops1 ∪ ops3) comp2 eff2
    comp3 = runExactPipeline pipeline1 comp1

    cast21' :: OpsCast (ops2 ∪ ops3) (ops1 ∪ ops3)
    cast21' = extendCast @ops2 @ops1 cast21

castPipelineHandler
  :: forall ops1 handler1 handler2 comp1 comp2 eff1 eff2 .
  ( Effect eff1
  , Effect eff2
  , BaseOps ops1
  , BaseOps handler1
  , BaseOps handler2
  )
  => OpsCast handler1 handler2
  -> Pipeline ops1 handler1 comp1 comp2 eff1 eff2
  -> Pipeline ops1 handler2 comp1 comp2 eff1 eff2
castPipelineHandler cast1 pipeline1 = Pipeline pipeline2
 where
  pipeline2 :: forall ops2 .
    (BaseOps ops2)
    => Computation (handler2 ∪ ops2) comp1 eff1
    -> Computation (ops1 ∪ ops2) comp2 eff2
  pipeline2 comp1 = runExactPipeline pipeline1 comp2
   where
    comp2 :: Computation (handler1 ∪ ops2) comp1 eff1
    comp2 = castComputation cast2 comp1

    cast2 :: OpsCast (handler1 ∪ ops2) (handler2 ∪ ops2)
    cast2 = extendCast @handler1 @handler2 cast1

composeExactPipelines
  :: forall ops1 ops2 handler1 handler2 comp1 comp2 comp3 eff1 eff2 eff3 .
  ( Effect eff1
  , Effect eff2
  , Effect eff3
  , BaseOps ops1
  , BaseOps ops2
  , BaseOps handler1
  , BaseOps handler2
  )
  => Pipeline (handler2 ∪ ops1) handler1 comp1 comp2 eff1 eff2
  -> Pipeline ops2 handler2 comp2 comp3 eff2 eff3
  -> Pipeline (ops1 ∪ ops2) (handler1 ∪ handler2) comp1 comp3 eff1 eff3
composeExactPipelines pipeline1 pipeline2 = Pipeline pipeline3
 where
  pipeline3 :: forall ops3 .
    (BaseOps ops3)
    => Computation ((handler1 ∪ handler2) ∪ ops3) comp1 eff1
    -> Computation ((ops1 ∪ ops2) ∪ ops3) comp3 eff3
  pipeline3 comp1 =
    castComputation cast comp4
     where
      comp1' :: Computation (handler1 ∪ handler2 ∪ ops3) comp1 eff1
      comp1' = castComputation cast comp1

      comp3 :: Computation ((handler2 ∪ ops1) ∪ (handler2 ∪ ops3)) comp2 eff2
      comp3 = runExactPipeline pipeline1 comp1'

      comp3' :: Computation (handler2 ∪ ops1 ∪ ops3) comp2 eff2
      comp3' = castComputation cast comp3

      comp4 :: Computation (ops2 ∪ ops1 ∪ ops3) comp3 eff3
      comp4 = runExactPipeline pipeline2 comp3'

runPipelineWithCast
  :: forall ops3 ops1 ops2 handler comp1 comp2 eff1 eff2 .
  ( Effect eff1
  , Effect eff2
  , BaseOps ops1
  , BaseOps ops2
  , BaseOps ops3
  , BaseOps handler
  )
  => OpsCast ops3 ops1
  -> OpsCast (handler ∪ ops3) ops2
  -> Pipeline ops1 handler comp1 comp2 eff1 eff2
  -> Computation ops2 comp1 eff1
  -> Computation ops3 comp2 eff2
runPipelineWithCast cast1 cast2 pipeline1 comp1
  = castComputation cast $ runExactPipeline pipeline2 comp2
  where
   pipeline2 :: Pipeline ops3 handler comp1 comp2 eff1 eff2
   pipeline2 = castPipelineOps cast1 pipeline1

   comp2 :: Computation (handler ∪ ops3) comp1 eff1
   comp2 = castComputation cast2 comp1

runPipeline
  :: forall ops3 ops1 ops2 handler comp1 comp2 eff1 eff2 .
  ( Effect eff1
  , Effect eff2
  , ops3 ⊇ ops1
  , (handler ∪ ops3) ⊇ ops2
  , BaseOps ops1
  , BaseOps ops2
  , BaseOps ops3
  , BaseOps handler
  )
  => Pipeline ops1 handler comp1 comp2 eff1 eff2
  -> Computation ops2 comp1 eff1
  -> Computation ops3 comp2 eff2
runPipeline = runPipelineWithCast
  (entailOps @ops3 @ops1)
  (entailOps @(handler ∪ ops3) @ops2)

composePipelinesWithCast
  :: forall ops1 ops2 ops3 handler1 handler2 handler3 comp1 comp2 comp3 eff1 eff2 eff3 .
  ( Effect eff1
  , Effect eff2
  , Effect eff3
  , BaseOps ops1
  , BaseOps ops2
  , BaseOps ops3
  , BaseOps handler1
  , BaseOps handler2
  , BaseOps handler3
  )
  => OpsCast (handler2 ∪ ops3) ops1
  -> OpsCast ops3 ops2
  -> OpsCast (handler1 ∪ handler2) handler3
  -> Pipeline ops1 handler1 comp1 comp2 eff1 eff2
  -> Pipeline ops2 handler2 comp2 comp3 eff2 eff3
  -> Pipeline ops3 handler3 comp1 comp3 eff1 eff3
composePipelinesWithCast cast1 cast2 cast3 pipeline1 pipeline2
  = castPipelineHandler cast3 $
    castPipelineOps cast $
    composeExactPipelines pipeline1' pipeline2'
  where
    pipeline1' :: Pipeline (handler2 ∪ ops3) handler1 comp1 comp2 eff1 eff2
    pipeline1' = castPipelineOps cast1 pipeline1

    pipeline2' :: Pipeline ops3 handler2 comp2 comp3 eff2 eff3
    pipeline2' = castPipelineOps cast2 pipeline2

composePipelines
  :: forall ops1 ops2 ops3 handler1 handler2 handler3 comp1 comp2 comp3 eff1 eff2 eff3 .
  ( Effect eff1
  , Effect eff2
  , Effect eff3
  , BaseOps ops1
  , BaseOps ops2
  , BaseOps ops3
  , BaseOps handler1
  , BaseOps handler2
  , BaseOps handler3
  , (handler2 ∪ ops3) ⊇ ops1
  , ops3 ⊇ ops2
  , (handler1 ∪ handler2) ⊇ handler3
  )
  => Pipeline ops1 handler1 comp1 comp2 eff1 eff2
  -> Pipeline ops2 handler2 comp2 comp3 eff2 eff3
  -> Pipeline ops3 handler3 comp1 comp3 eff1 eff3
composePipelines = composePipelinesWithCast
  (entailOps @(handler2 ∪ ops3) @ops1)
  (entailOps @ops3 @ops2)
  (entailOps @(handler1 ∪ handler2) @handler3)
