{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Effect.Cast
  ( Cast (..)
  , OpsCast (..)
  , castOps
  , runCast
  , composeCast
  , extendNoEffCast
  , weakenLeftCast
  , weakenRightCast
  , distributeLeftCast
  , distributeRightCast
  , castComputation
  , castHandler
  , swapOps
  , weakenComputation
  )
where

import Control.Effect.Base
  ( NoEff
  , Union
  , Effect
  , LiftEff
  , EffOps (..)
  , Handler (..)
  , FreeEff (..)
  , OpsConstraint
  , Computation (..)
  )

data Cast p = p => Cast

data OpsCast ops1 ops2 = OpsCast
  (forall eff . OpsConstraint ops1 eff => Cast (OpsConstraint ops2 eff))

runCast
  :: forall eff ops1 ops2 r .
  ( OpsConstraint ops1 eff )
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
      (OpsConstraint ops1 eff)
      => Cast (OpsConstraint ops3 eff)
    cast3 = runCast @eff cast1 $ runCast @eff cast2 Cast

extendNoEffCast
  :: forall ops1 ops2 .
  ( EffOps ops1
  , EffOps ops2
  )
  => OpsCast ops1 ops2
  -> OpsCast ops1 (Union NoEff ops2)
extendNoEffCast cast1 = OpsCast cast2
  where
    cast2
      :: forall eff .
      (OpsConstraint ops1 eff)
      => Cast (OpsConstraint (Union NoEff ops2) eff)
    cast2 = runCast @eff cast1 Cast

weakenRightCast
  :: forall ops1 ops2 ops3 .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  )
  => OpsCast ops1 (Union ops2 ops3)
  -> OpsCast ops1 ops2
weakenRightCast cast1 = OpsCast cast2
  where
    cast2
      :: forall eff .
      (OpsConstraint ops1 eff)
      => Cast (OpsConstraint ops2 eff)
    cast2 = runCast @eff cast1 Cast

weakenLeftCast
  :: forall ops1 ops2 ops3 .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  )
  => OpsCast ops1 ops2
  -> OpsCast (Union ops1 ops3) ops2
weakenLeftCast (OpsCast cast) = OpsCast cast

distributeRightCast
  :: forall ops1 ops2 ops3 ops4 .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  , EffOps ops4
  )
  => OpsCast ops1 (Union (Union ops2 ops3) ops4)
  -> OpsCast ops1 (Union ops2 (Union ops3 ops4))
distributeRightCast cast1 = OpsCast cast2
  where
    cast2
      :: forall eff .
      (OpsConstraint ops1 eff)
      => Cast
        ( ( OpsConstraint ops4 eff
          , OpsConstraint ops3 eff
          )
        , OpsConstraint ops2 eff
        )
    cast2 = runCast @eff cast1 Cast

distributeLeftCast
  :: forall ops1 ops2 ops3 ops4 .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  , EffOps ops4
  )
  => OpsCast (Union (Union ops1 ops2) ops3) ops4
  -> OpsCast (Union ops1 (Union ops2 ops3)) ops4
distributeLeftCast (OpsCast cast) = OpsCast cast

castComputation
  :: forall eff ops1 ops2 comp .
  ( EffOps ops1
  , EffOps ops2
  , Effect eff
  )
  => Computation ops1 comp eff
  -> OpsCast ops2 ops1
  -> Computation ops2 comp eff
castComputation comp1 cast = Computation comp2
  where
    comp2 :: forall eff' .
      (Effect eff')
      => LiftEff eff eff'
      -> (OpsConstraint ops2 eff' => comp eff')
    comp2 lifter = runCast @eff' cast $ runComp comp1 lifter

castHandler
  :: forall eff1 eff2 ops1 ops2 handler .
  ( EffOps ops1
  , EffOps ops2
  , Effect eff1
  , Effect eff2
  )
  => Handler ops1 handler eff1 eff2
  -> OpsCast ops2 ops1
  -> Handler ops2 handler eff1 eff2
castHandler (Handler lifter handler) cast =
  Handler lifter handler2
    where
      handler2 :: Computation ops2 (Operation handler) eff1
      handler2 = castComputation handler cast

swapOps
  :: forall ops1 ops2 comp eff .
  ( EffOps ops1
  , EffOps ops2
  , Effect eff
  )
  => Computation (Union ops1 ops2) comp eff
  -> Computation (Union ops2 ops1) comp eff
swapOps comp = castComputation comp $ OpsCast Cast

weakenComputation
  :: forall ops1 ops2 comp eff .
  ( EffOps ops1
  , EffOps ops2
  , Effect eff
  )
  => Computation ops1 comp eff
  -> Computation (Union ops2 ops1) comp eff
weakenComputation comp = castComputation comp $ OpsCast Cast