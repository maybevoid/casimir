{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Effect.Computation.Cast

where

import Control.Effect.Base
import Control.Effect.Computation.Class

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
opsCast cast = OpsCast cast

runCast
  :: forall eff ops1 ops2 r .
  ( Effect eff, OpsConstraint ops1 eff )
  => OpsCast ops1 ops2
  -> (OpsConstraint ops2 eff => r)
  -> r
runCast (OpsCast cast) res =
  case cast @eff of
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
castOps cast ops = bindConstraint ops $ runCast @eff cast captureOps

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
castComputation cast comp = Computation $
  \ liftEff ops ->
    runComp comp liftEff $ castOps cast ops

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
castHandler cast (Handler liftEff handler)
  = Handler liftEff $ castComputation cast handler