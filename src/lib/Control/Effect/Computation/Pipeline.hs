{-# LANGUAGE QuantifiedConstraints #-}

module Control.Effect.Computation.Pipeline
where

import Control.Effect.Base
import Control.Effect.Computation.Class
import Control.Effect.Computation.Cast

newtype Pipeline ops1 handler eff1 eff2 comp1 comp2
  = Pipeline {
      runPipeline
        :: forall ops2 .
        (EffOps ops2)
        => Computation (handler ∪ ops2) comp1 eff1
        -> Computation (ops1 ∪ ops2) comp2 eff2
  }

data TransformerHandler t handler eff = TransformerHandler {
  tCoOpHandler :: Operation handler (t eff),
  tLiftEff :: LiftEff eff (t eff),
  tUnliftEff :: LiftEff (t eff) eff
}

type SimplePipeline ops handler eff comp
  = Pipeline ops handler eff eff comp comp

type GenericPipeline ops handler eff
  = forall comp .
    (EffFunctor comp)
    => SimplePipeline ops handler eff comp

handlerToPipeline
  :: forall ops1 handler eff1 eff2 .
  ( Effect eff1
  , Effect eff2
  , EffOps ops1
  , EffOps handler
  )
  => Handler ops1 handler eff1 eff2
  -> (forall comp .
      (EffFunctor comp)
      => Pipeline ops1 handler eff2 eff1 comp comp)
handlerToPipeline handler1 = Pipeline pipeline
 where
  pipeline :: forall ops2 comp .
    (EffOps ops2, EffFunctor comp)
    => Computation (handler ∪ ops2) comp eff2
    -> Computation (ops1 ∪ ops2) comp eff1
  pipeline comp1 = bindHandlerWithCast @(ops1 ∪ ops2)
    handler1 comp1
    cast
    cast

{-# INLINE transformerPipeline #-}
transformerPipeline
  :: forall t ops1 handler eff1 .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  , (forall eff . (Effect eff) => Effect (t eff))
  )
  => Computation ops1 (TransformerHandler t handler) eff1
  -> GenericPipeline ops1 handler eff1
transformerPipeline handler1 = Pipeline pipeline
 where
  {-# INLINE pipeline #-}
  pipeline :: forall ops2 comp .
    (EffOps ops2, EffFunctor comp)
    => Computation (handler ∪ ops2) comp eff1
    -> Computation (ops1 ∪ ops2) comp eff1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall eff2 . (Effect eff2)
      => LiftEff eff1 eff2
      -> Operation (ops1 ∪ ops2) eff2
      -> comp eff2
    comp2 lift12 (UnionOps ops1 ops2) = applyLift unliftT comp3
     where
      TransformerHandler coopHandler liftT unliftT
        = runComp handler1 lift12 ops1

      comp3 :: comp (t eff2)
      comp3 = runComp comp1 (joinLift lift12 liftT) $
        UnionOps coopHandler $ applyLift liftT ops2

castPipeline
  :: forall ops1 ops2 handler eff1 eff2 comp1 comp2 .
  ( Effect eff1
  , Effect eff2
  , EffOps ops1
  , EffOps ops2
  , EffOps handler
  )
  => ops2 ⊇ ops1
  -> Pipeline ops1 handler eff1 eff2 comp1 comp2
  -> Pipeline ops2 handler eff1 eff2 comp1 comp2
castPipeline cast21 pipeline1 = Pipeline pipeline2
 where
  pipeline2 :: forall ops3 .
    (EffOps ops3)
    => Computation (handler ∪ ops3) comp1 eff1
    -> Computation (ops2 ∪ ops3) comp2 eff2
  pipeline2 comp1 = comp2
   where
    comp2 :: Computation (ops2 ∪ ops3) comp2 eff2
    comp2 = castComputation cast21' comp3

    comp3 :: Computation (ops1 ∪ ops3) comp2 eff2
    comp3 = runPipeline pipeline1 comp1

    cast21' :: (ops2 ∪ ops3) ⊇ (ops1 ∪ ops3)
    cast21' = extendCast @ops2 @ops1 cast21

castPipelineHandler
  :: forall ops1 handler1 handler2 eff1 eff2 comp1 comp2 .
  ( Effect eff1
  , Effect eff2
  , EffOps ops1
  , EffOps handler1
  , EffOps handler2
  )
  => handler1 ⊇ handler2
  -> Pipeline ops1 handler1 eff1 eff2 comp1 comp2
  -> Pipeline ops1 handler2 eff1 eff2 comp1 comp2
castPipelineHandler cast1 pipeline1 = Pipeline pipeline2
 where
  pipeline2 :: forall ops2 .
    (EffOps ops2)
    => Computation (handler2 ∪ ops2) comp1 eff1
    -> Computation (ops1 ∪ ops2) comp2 eff2
  pipeline2 comp1 = runPipeline pipeline1 comp2
   where
    comp2 :: Computation (handler1 ∪ ops2) comp1 eff1
    comp2 = castComputation cast2 comp1

    cast2 :: (handler1 ∪ ops2) ⊇ (handler2 ∪ ops2)
    cast2 = extendCast @handler1 @handler2 cast1

composePipelines
  :: forall ops1 ops2 handler1 handler2 eff1 eff2 eff3 comp1 comp2 comp3 .
  ( Effect eff1
  , Effect eff2
  , Effect eff3
  , EffOps ops1
  , EffOps ops2
  , EffOps handler1
  , EffOps handler2
  )
  => Pipeline (handler2 ∪ ops1) handler1 eff1 eff2 comp1 comp2
  -> Pipeline ops2 handler2 eff2 eff3 comp2 comp3
  -> Pipeline (ops1 ∪ ops2) (handler1 ∪ handler2) eff1 eff3 comp1 comp3
composePipelines pipeline1 pipeline2 = Pipeline pipeline3
 where
  pipeline3 :: forall ops3 .
    (EffOps ops3)
    => Computation ((handler1 ∪ handler2) ∪ ops3) comp1 eff1
    -> Computation ((ops1 ∪ ops2) ∪ ops3) comp3 eff3
  pipeline3 comp1 =
    castComputation cast comp4
     where
      comp1' :: Computation (handler1 ∪ handler2 ∪ ops3) comp1 eff1
      comp1' = castComputation cast comp1

      comp3 :: Computation ((handler2 ∪ ops1) ∪ (handler2 ∪ ops3)) comp2 eff2
      comp3 = runPipeline pipeline1 comp1'

      comp3' :: Computation (handler2 ∪ ops1 ∪ ops3) comp2 eff2
      comp3' = castComputation cast comp3

      comp4 :: Computation (ops2 ∪ ops1 ∪ ops3) comp3 eff3
      comp4 = runPipeline pipeline2 comp3'

runPipelineWithCast
  :: forall ops3 ops1 ops2 handler comp1 comp2 eff1 eff2 .
  ( Effect eff1
  , Effect eff2
  , EffOps ops1
  , EffOps ops2
  , EffOps ops3
  , EffOps handler
  )
  => Pipeline ops1 handler eff1 eff2 comp1 comp2
  -> Computation ops2 comp1 eff1
  -> ops3 ⊇ ops1
  -> (handler ∪ ops3) ⊇ ops2
  -> Computation ops3 comp2 eff2
runPipelineWithCast pipeline1 comp1 cast1 cast2
  = castComputation cast $ runPipeline pipeline2 comp2
  where
   pipeline2 :: Pipeline ops3 handler eff1 eff2 comp1 comp2
   pipeline2 = castPipeline cast1 pipeline1

   comp2 :: Computation (handler ∪ ops3) comp1 eff1
   comp2 = castComputation cast2 comp1

composePipelinesWithCast
  :: forall ops1 ops2 ops3 handler1 handler2 handler3 eff1 eff2 eff3 comp1 comp2 comp3 .
  ( Effect eff1
  , Effect eff2
  , Effect eff3
  , EffOps ops1
  , EffOps ops2
  , EffOps ops3
  , EffOps handler1
  , EffOps handler2
  , EffOps handler3
  )
  => Pipeline ops1 handler1 eff1 eff2 comp1 comp2
  -> Pipeline ops2 handler2 eff2 eff3 comp2 comp3
  -> (handler2 ∪ ops3) ⊇ ops1
  -> ops3 ⊇ ops2
  -> (handler1 ∪ handler2) ⊇ handler3
  -> Pipeline ops3 handler3 eff1 eff3 comp1 comp3
composePipelinesWithCast pipeline1 pipeline2 cast1 cast2 cast3
  = castPipelineHandler cast3 $
    castPipeline cast $
    composePipelines pipeline1' pipeline2'
  where
    pipeline1' :: Pipeline (handler2 ∪ ops3) handler1 eff1 eff2 comp1 comp2
    pipeline1' = castPipeline cast1 pipeline1

    pipeline2' :: Pipeline ops3 handler2 eff2 eff3 comp2 comp3
    pipeline2' = castPipeline cast2 pipeline2
