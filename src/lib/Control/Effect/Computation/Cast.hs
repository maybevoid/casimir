{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Effect.Computation.Cast

where

import Control.Effect.Base
import Control.Effect.Computation.Class
import Control.Effect.Computation.Handler

data Cast p = p => Cast

data OpsCast ops1 ops2 = OpsCast
  (forall eff . (Effect eff, OpsConstraint ops1 eff) => Cast (OpsConstraint ops2 eff))

cast :: forall p . p => Cast p
cast = Cast

opsCast
  :: forall ops1 ops2 .
  ( EffOps ops1
  , EffOps ops2
  )
  => (forall eff .
      (Effect eff, OpsConstraint ops1 eff)
      => Cast (OpsConstraint ops2 eff))
  -> OpsCast ops1 ops2
opsCast caster = OpsCast caster

runCast
  :: forall eff ops1 ops2 r .
  ( Effect eff, OpsConstraint ops1 eff )
  => OpsCast ops1 ops2
  -> (OpsConstraint ops2 eff => r)
  -> r
runCast (OpsCast caster) res =
  case caster @eff of
    Cast -> res

castOps
  :: forall eff ops1 ops2 .
  ( Effect eff
  , EffOps ops1
  , EffOps ops2
  )
  => OpsCast ops1 ops2
  -> Operation ops1 eff
  -> Operation ops2 eff
castOps caster ops = bindConstraint ops $ runCast @eff caster captureOps

composeCast
  :: forall ops1 ops2 ops3.
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  )
  => OpsCast ops1 ops2
  -> OpsCast ops2 ops3
  -> OpsCast ops1 ops3
composeCast cast1 cast2 = OpsCast cast3
  where
    cast3
      :: forall eff .
      (Effect eff, OpsConstraint ops1 eff)
      => Cast (OpsConstraint ops3 eff)
    cast3 = runCast @eff cast1 $ runCast @eff cast2 Cast

castComputation
  :: forall ops1 ops2 comp eff .
  ( Effect eff
  , EffOps ops1
  , EffOps ops2
  )
  => OpsCast ops1 ops2
  -> Computation ops2 comp eff
  -> Computation ops1 comp eff
castComputation caster comp = Computation $
  \ liftEff ops ->
    runComp comp liftEff $ castOps caster ops

castHandler
  :: forall ops1 ops2 handler eff1 eff2 .
  ( Effect eff1
  , Effect eff2
  , EffOps ops1
  , EffOps ops2
  )
  => OpsCast ops1 ops2
  -> Handler ops2 handler eff1 eff2
  -> Handler ops1 handler eff1 eff2
castHandler caster (Handler liftEff handler)
  = Handler liftEff $ castComputation caster handler

composeHandlersWithCast
  :: forall
    ops1 ops2 ops3
    handler1 handler2
    eff1 eff2 eff3 .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  , EffOps handler1
  , EffOps handler2
  , Effect eff1
  , Effect eff2
  , Effect eff3
  )
  => Handler ops1 handler1 eff1 eff2
  -> Handler ops2 handler2 eff2 eff3
  -> OpsCast ops3 ops1
  -> OpsCast (Union handler1 ops3) ops2
  -> Handler ops3 (Union handler1 handler2) eff1 eff3
composeHandlersWithCast handler1 handler2 cast31 cast32 =
  composeExactHandlers
    (castHandler cast31 handler1) $
    castHandler cast32 handler2

bindHandlerWithCast
  :: forall ops3 ops1 ops2 handler eff1 eff2 r .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  , EffOps handler
  , Effect eff1
  , Effect eff2
  )
  => Handler ops1 handler eff1 eff2
  -> Computation ops2 r eff2
  -> OpsCast ops3 ops1
  -> OpsCast (Union handler ops3) ops2
  -> Computation ops3 r eff1
bindHandlerWithCast handler comp cast31 cast32 =
  bindExactHandler
    (castHandler cast31 handler) $
    castComputation cast32 comp
