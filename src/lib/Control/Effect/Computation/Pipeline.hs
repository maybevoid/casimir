
module Control.Effect.Computation.Pipeline
where

import Control.Effect.Base
import Control.Effect.Computation.Class
import Control.Effect.Computation.Cast

type Pipeline ops1 handler eff comp1 comp2
  = forall ops2 .
    (EffOps ops2)
    => Computation (Union handler ops2) comp1 eff
    -> Computation (Union ops1 ops2) comp2 eff

type GenericPipeline ops handler eff
  = forall comp .
    (EffFunctor comp)
    => Pipeline ops handler eff comp comp

handlerToPipeline
  :: forall ops1 handler eff .
  ( Effect eff
  , EffOps ops1
  , EffOps handler
  )
  => (Handler ops1 handler eff eff)
  -> GenericPipeline ops1 handler eff
handlerToPipeline handler1 = pipeline
 where
  pipeline :: forall ops2 comp .
    (EffOps ops2)
    => Computation (Union handler ops2) comp eff
    -> Computation (Union ops1 ops2) comp eff
  pipeline comp1 = bindHandlerWithCast @(Union ops1 ops2)
    handler1 comp1
    (opsCast cast)
    (opsCast cast)

castPipeline
  :: forall ops1 ops2 handler eff comp1 comp2 .
  ( Effect eff
  , EffOps ops1
  , EffOps ops2
  , EffOps handler
  )
  => OpsCast ops2 ops1
  -> Pipeline ops1 handler eff comp1 comp2
  -> Pipeline ops2 handler eff comp1 comp2
castPipeline cast21 pipeline1 = pipeline2
 where
  pipeline2 :: forall ops3 .
    (EffOps ops3)
    => Computation (Union handler ops3) comp1 eff
    -> Computation (Union ops2 ops3) comp2 eff
  pipeline2 comp1 = comp2
   where
    comp2 :: Computation (Union ops2 ops3) comp2 eff
    comp2 = castComputation cast21' comp3

    comp3 :: Computation (Union ops1 ops3) comp2 eff
    comp3 = pipeline1 comp1

    cast21' :: OpsCast (Union ops2 ops3) (Union ops1 ops3)
    cast21' = extendCast cast21

composePipelines
  :: forall ops1 ops2 handler1 handler2 eff comp1 comp2 comp3 .
  ( Effect eff
  , EffOps ops1
  , EffOps ops2
  , EffOps handler1
  , EffOps handler2
  )
  => Pipeline ops1 handler1 eff comp1 comp2
  -> Pipeline ops2 handler2 eff comp2 comp3
  -> Pipeline (Union ops1 ops2) (Union handler1 handler2) eff comp1 comp3
composePipelines pipeline1 pipeline2 = pipeline3
 where
  pipeline3 :: forall ops3 .
    (EffOps ops3)
    => Computation (Union (Union handler1 handler2) ops3) comp1 eff
    -> Computation (Union (Union ops1 ops2) ops3) comp3 eff
  pipeline3 comp1 = castComputation (opsCast cast) comp4
   where
    comp1' :: Computation (Union handler1 (Union handler2 ops3)) comp1 eff
    comp1' = castComputation (opsCast cast) comp1

    comp3 :: Computation (Union ops1 (Union handler2 ops3)) comp2 eff
    comp3 = pipeline1 comp1'

    comp3' :: Computation (Union handler2 (Union ops1 ops3)) comp2 eff
    comp3' = castComputation (opsCast cast) comp3

    comp4 :: Computation (Union ops2 (Union ops1 ops3)) comp3 eff
    comp4 = pipeline2 comp3'