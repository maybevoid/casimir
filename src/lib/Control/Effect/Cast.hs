{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Effect.Cast
  ( Cast (..)
  , CastOps (..)
  , runCast
  , extendNoOpCast
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

import Control.Effect.Union (Union (..))
import Control.Effect.Ops.NoOp (NoOp (..))

import Control.Effect.Class
  ( Effect
  , EffOps
  , LiftEff
  , Handler (..)
  , EffConstraint
  , Computation (..)
  )

data Cast p = p => Cast

data CastOps ops1 ops2 = CastOps
  (forall eff . EffConstraint ops1 eff => Cast (EffConstraint ops2 eff))

runCast
  :: forall eff ops1 ops2 r .
  ( EffConstraint ops1 eff )
  => CastOps ops1 ops2
  -> (EffConstraint ops2 eff => r)
  -> r
runCast (CastOps cast) res =
  case cast @eff of
    Cast -> res

extendNoOpCast
  :: forall ops1 ops2 .
  ( EffOps ops1
  , EffOps ops2
  )
  => CastOps ops1 ops2
  -> CastOps ops1 (Union NoOp ops2)
extendNoOpCast (CastOps cast1) = CastOps cast2
  where
    cast2
      :: forall eff .
      (EffConstraint ops1 eff)
      => Cast (EffConstraint (Union NoOp ops2) eff)
    cast2 = case cast1 @eff of
      Cast -> Cast

weakenRightCast
  :: forall ops1 ops2 ops3 .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  )
  => CastOps ops1 (Union ops2 ops3)
  -> CastOps ops1 ops2
weakenRightCast (CastOps cast1) = CastOps cast2
  where
    cast2
      :: forall eff .
      (EffConstraint ops1 eff)
      => Cast (EffConstraint ops2 eff)
    cast2 = case cast1 @eff of
      Cast -> Cast

weakenLeftCast
  :: forall ops1 ops2 ops3 .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  )
  => CastOps ops1 ops2
  -> CastOps (Union ops1 ops3) ops2
weakenLeftCast (CastOps cast) = CastOps cast

distributeRightCast
  :: forall ops1 ops2 ops3 ops4 .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  , EffOps ops4
  )
  => CastOps ops1 (Union (Union ops2 ops3) ops4)
  -> CastOps ops1 (Union ops2 (Union ops3 ops4))
distributeRightCast (CastOps cast1) = CastOps cast2
  where
    cast2
      :: forall eff .
      (EffConstraint ops1 eff)
      => Cast
        ( EffConstraint ops2 eff
        , ( EffConstraint ops3 eff
          , EffConstraint ops4 eff
          )
        )
    cast2 = case cast1 @eff of
      Cast -> Cast

distributeLeftCast
  :: forall ops1 ops2 ops3 ops4 .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  , EffOps ops4
  )
  => CastOps (Union (Union ops1 ops2) ops3) ops4
  -> CastOps (Union ops1 (Union ops2 ops3)) ops4
distributeLeftCast (CastOps cast) = CastOps cast

castComputation
  :: forall eff ops1 ops2 comp .
  ( EffOps ops1
  , EffOps ops2
  , Effect eff
  )
  => Computation ops1 comp eff
  -> CastOps ops2 ops1
  -> Computation ops2 comp eff
castComputation comp1 cast = Computation comp2
  where
    comp2 :: forall eff' .
      (Effect eff')
      => LiftEff eff eff'
      -> (EffConstraint ops2 eff' => comp eff')
    comp2 lifter = runCast @eff' cast $ runComp comp1 lifter

castHandler
  :: forall eff1 eff2 ops1 ops2 handler .
  ( EffOps ops1
  , EffOps ops2
  , Effect eff1
  , Effect eff2
  )
  => Handler ops1 handler eff1 eff2
  -> CastOps ops2 ops1
  -> Handler ops2 handler eff1 eff2
castHandler (Handler lifter handler) cast =
  Handler lifter handler2
    where
      handler2 :: Computation ops2 handler eff1
      handler2 = castComputation handler cast

swapOps
  :: forall ops1 ops2 comp eff .
  ( EffOps ops1
  , EffOps ops2
  , Effect eff
  )
  => Computation (Union ops1 ops2) comp eff
  -> Computation (Union ops2 ops1) comp eff
swapOps comp = castComputation comp $ CastOps Cast

weakenComputation
  :: forall ops1 ops2 comp eff .
  ( EffOps ops1
  , EffOps ops2
  , Effect eff
  )
  => Computation ops1 comp eff
  -> Computation (Union ops2 ops1) comp eff
weakenComputation comp = castComputation comp $ CastOps Cast