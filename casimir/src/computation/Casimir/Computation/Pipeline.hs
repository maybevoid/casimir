
module Casimir.Computation.Pipeline
  -- ( Pipeline (..)
  -- , BasePipeline
  -- , TransformerHandler (..)
  -- , SimplePipeline
  -- , GenericPipeline
  -- , opsHandlerToPipeline
  -- , transformePipeline
  -- , castPipelineOps
  -- , castPipelineHandler
  -- , composePipelines
  -- , composeExactPipelines
  -- , runPipeline
  -- , runPipelineWithCast
  -- , composePipelinesWithCast
  -- )
where

import Casimir.Base
import Casimir.Computation.Cast
import Casimir.Computation.Handler
import Casimir.Computation.Computation

data Pipeline lift ops1 handler comp1 comp2 m1 m2
  = Pipeline
  { unPipeline
      :: forall ops2
       . ( Effects ops2
         , EffFunctor lift (ops2)
         )
      => Computation lift (handler ∪ ops2) comp1 m1
      -> Computation lift (ops1 ∪ ops2) comp2 m2
  }

runExactPipeline
  :: forall lift ops1 ops2 handler comp1 comp2 m1 m2
   . ( Effects ops2
     , EffFunctor lift (ops2)
     )
  => Pipeline lift ops1 handler comp1 comp2 m1 m2
  -> Computation lift (handler ∪ ops2) comp1 m1
  -> Computation lift (ops1 ∪ ops2) comp2 m2
runExactPipeline (Pipeline runner) comp = runner @ops2 comp

data TransformerHandler t handler m = TransformerHandler {
  tCoOpHandler :: handler (t m),
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
     , Effects ops1
     , Effects handler
     , LiftMonoid lift
      , EffFunctor lift (handler)
     )
  => OpsHandler lift ops1 handler m
  -> (forall comp
       . (EffFunctor lift comp)
      => SimplePipeline lift ops1 handler comp m )
opsHandlerToPipeline handler1 = Pipeline pipeline
 where
  pipeline :: forall ops2 comp
     . ( Effects ops2
       , EffFunctor lift comp
       )
    => Computation lift (handler ∪ ops2) comp m
    -> Computation lift (ops1 ∪ ops2) comp m
  pipeline comp1 = bindOpsHandler @(ops1 ∪ ops2) @ops1
    handler1 comp1

{-# INLINE transformePipeline #-}
transformePipeline
  :: forall t ops1 handler m1 .
  ( Monad m1
  , Effects ops1
  , Effects handler
  , (forall m . (Monad m) => Monad (t m))
  )
  => Computation Lift ops1 (TransformerHandler t handler) m1
  -> GenericPipeline Lift ops1 handler m1
transformePipeline handler1 = Pipeline pipeline
 where
  {-# INLINE pipeline #-}
  pipeline :: forall ops2 comp .
    ( Effects ops2
    , EffFunctor Lift comp
    , EffFunctor Lift (ops2)
    )
    => BaseComputation (handler ∪ ops2) comp m1
    -> BaseComputation (ops1 ∪ ops2) comp m1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall m2 . (Monad m2)
      => Lift m1 m2
      -> (ops1 ∪ ops2) m2
      -> comp m2
    comp2 lift12 (Union ops1 ops2) = effmap unliftT comp3
     where
      TransformerHandler coopHandler liftT unliftT
        = runComp handler1 lift12 ops1

      comp3 :: comp (t m2)
      comp3 = runComp comp1 (joinLift lift12 liftT) $
        Union coopHandler $ effmap liftT ops2

castPipelineOps
  :: forall ops1 ops2 lift handler comp1 comp2 m1 m2  .
  ( Monad m1
  , Monad m2
  , Effects ops1
  , Effects ops2
  , Effects handler
  )
  => CastDict ops2 ops1
  -> Pipeline lift ops1 handler comp1 comp2 m1 m2
  -> Pipeline lift ops2 handler comp1 comp2 m1 m2
castPipelineOps cast pipeline1 = Pipeline pipeline2
 where
  pipeline2 :: forall ops3
     . ( Effects ops3
       , EffFunctor lift (ops3)
       )
    => Computation lift (handler ∪ ops3) comp1 m1
    -> Computation lift (ops2 ∪ ops3) comp2 m2
  pipeline2 comp1 = comp2
   where
    comp2 :: Computation lift (ops2 ∪ ops3) comp2 m2
    comp2 = castComputationWithDict cast21 comp3

    comp3 :: Computation lift (ops1 ∪ ops3) comp2 m2
    comp3 = runExactPipeline pipeline1 comp1

    cast21 :: CastDict (ops2 ∪ ops3) (ops1 ∪ ops3)
    cast21 = extendCast @ops2 @ops1 $ cast

castPipelineHandler
  :: forall ops1 lift handler1 handler2 comp1 comp2 m1 m2
   . ( Monad m1
     , Monad m2
     , Effects ops1
     , Effects handler1
     , Effects handler2
     , LiftMonoid lift
     , EffFunctor lift (handler1)
     )
  => CastDict handler1 handler2
  -> Pipeline lift ops1 handler1 comp1 comp2 m1 m2
  -> Pipeline lift ops1 handler2 comp1 comp2 m1 m2
castPipelineHandler cast1 pipeline1 = Pipeline pipeline2
 where
  pipeline2
    :: forall ops2
     . ( Effects ops2
       , EffFunctor lift (ops2)
       )
    => Computation lift (handler2 ∪ ops2) comp1 m1
    -> Computation lift (ops1 ∪ ops2) comp2 m2
  pipeline2 comp1 =
    runExactPipeline pipeline1 comp2
   where
    comp2 :: Computation lift (handler1 ∪ ops2) comp1 m1
    comp2 = castComputationWithDict cast2 comp1

    cast2 :: CastDict (handler1 ∪ ops2) (handler2 ∪ ops2)
    cast2 = extendCast @handler1 @handler2 cast1

composeExactPipelines
  :: forall ops1 ops2 lift handler1 handler2 comp1 comp2 comp3 m1 m2 m3 .
  ( Monad m1
  , Monad m2
  , Monad m3
  , LiftMonoid lift
  , Effects ops1
  , Effects ops2
  , Effects handler1
  , Effects handler2
  , EffFunctor lift (ops1)
  , EffFunctor lift (handler1)
  , EffFunctor lift (handler2)
  )
  => Pipeline lift (handler2 ∪ ops1) handler1 comp1 comp2 m1 m2
  -> Pipeline lift ops2 handler2 comp2 comp3 m2 m3
  -> Pipeline lift (ops1 ∪ ops2) (handler1 ∪ handler2) comp1 comp3 m1 m3
composeExactPipelines pipeline1 pipeline2 = Pipeline pipeline3
 where
  pipeline3 :: forall ops3
     . ( Effects ops3
       , EffFunctor lift (ops3)
       , EffFunctor lift (handler2 ∪ ops3)
       )
    => Computation lift ((handler1 ∪ handler2) ∪ ops3) comp1 m1
    -> Computation lift ((ops1 ∪ ops2) ∪ ops3) comp3 m3
  pipeline3 comp1 =
    castComputation  comp4
     where
      comp1' :: Computation lift (handler1 ∪ handler2 ∪ ops3) comp1 m1
      comp1' = castComputation comp1

      comp3 :: Computation lift ((handler2 ∪ ops1) ∪ (handler2 ∪ ops3)) comp2 m2
      comp3 = runExactPipeline pipeline1 comp1'

      comp3' :: Computation lift (handler2 ∪ ops1 ∪ ops3) comp2 m2
      comp3' = castComputation comp3

      comp4 :: Computation lift (ops2 ∪ ops1 ∪ ops3) comp3 m3
      comp4 = runExactPipeline pipeline2 comp3'

runPipelineWithCast
  :: forall ops3 ops1 ops2 lift handler comp1 comp2 m1 m2 .
  ( Monad m1
  , Monad m2
  , Effects ops1
  , Effects ops2
  , Effects ops3
  , Effects handler
  , EffFunctor lift (ops3)
  )
  => CastDict ops3 ops1
  -> CastDict (handler ∪ ops3) ops2
  -> Pipeline lift ops1 handler comp1 comp2 m1 m2
  -> Computation lift ops2 comp1 m1
  -> Computation lift ops3 comp2 m2
runPipelineWithCast cast1 cast2 pipeline1 comp1 =
  castComputation $
    runExactPipeline pipeline2 comp2
 where
   pipeline2 :: Pipeline lift ops3 handler comp1 comp2 m1 m2
   pipeline2 = castPipelineOps cast1 pipeline1

   comp2 :: Computation lift (handler ∪ ops3) comp1 m1
   comp2 = castComputationWithDict cast2 comp1

runPipeline
  :: forall ops3 ops1 ops2 lift handler comp1 comp2 m1 m2 .
  ( Monad m1
  , Monad m2
  , ops3 ⊇ ops1
  , (handler ∪ ops3) ⊇ ops2
  , Effects ops1
  , Effects ops2
  , Effects ops3
  , Effects handler
  , EffFunctor lift (ops3)
  )
  => Pipeline lift ops1 handler comp1 comp2 m1 m2
  -> Computation lift ops2 comp1 m1
  -> Computation lift ops3 comp2 m2
runPipeline = runPipelineWithCast
  (castDict @ops3 @ops1)
  (castDict @(handler ∪ ops3) @ops2)

composePipelinesWithCast
  :: forall ops1 ops2 ops3 lift handler1 handler2 handler3
      comp1 comp2 comp3 m1 m2 m3
   . ( Monad m1
     , Monad m2
     , Monad m3
     , LiftMonoid lift
     , Effects ops1
     , Effects ops2
     , Effects ops3
     , Effects handler1
     , Effects handler2
     , Effects handler3
     , EffFunctor lift (ops3)
     , EffFunctor lift (handler1)
     , EffFunctor lift (handler2)
     , EffFunctor lift ((handler1 ∪ handler2))
     )
  => CastDict (handler2 ∪ ops3) ops1
  -> CastDict ops3 ops2
  -> CastDict (handler1 ∪ handler2) handler3
  -> Pipeline lift ops1 handler1 comp1 comp2 m1 m2
  -> Pipeline lift ops2 handler2 comp2 comp3 m2 m3
  -> Pipeline lift ops3 handler3 comp1 comp3 m1 m3
composePipelinesWithCast cast1 cast2 cast3 pipeline1 pipeline2
  = castPipelineHandler cast3 $
    castPipelineOps (castDict @ops3) $
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
     , Effects ops1
     , Effects ops2
     , Effects ops3
     , Effects handler1
     , Effects handler2
     , Effects handler3
     , (handler2 ∪ ops3) ⊇ ops1
     , ops3 ⊇ ops2
     , (handler1 ∪ handler2) ⊇ handler3
     , EffFunctor lift (ops3)
     , EffFunctor lift (handler1)
     , EffFunctor lift (handler2)
     , EffFunctor lift ((handler1 ∪ handler2))
     )
  => Pipeline lift ops1 handler1 comp1 comp2 m1 m2
  -> Pipeline lift ops2 handler2 comp2 comp3 m2 m3
  -> Pipeline lift ops3 handler3 comp1 comp3 m1 m3
composePipelines = composePipelinesWithCast
  (castDict @(handler2 ∪ ops3) @ops1)
  (castDict @ops3 @ops2)
  (castDict @(handler1 ∪ handler2) @handler3)
