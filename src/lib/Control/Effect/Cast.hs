{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Effect.Cast where

import Control.Effect.Union
import Control.Effect.Class

data Cast p = p => Cast

castComputation :: forall ops1 ops2 comp eff1 .
  ( EffRow ops1
  , EffRow ops2
  , Effect eff1
  )
  => Computation ops1 comp eff1
  -> (forall eff2 . EffConstraint ops2 eff2 => Cast (EffConstraint ops1 eff2))
  -> Computation ops2 comp eff1
castComputation comp1 cast = Computation comp2
  where
    comp2 :: forall eff3 .
      (Effect eff3)
      => LiftEff eff1 eff3
      -> (EffConstraint ops2 eff3 => comp eff3)
    comp2 lifter = case cast @eff3 of
      Cast -> runComp comp1 lifter

swapOps :: forall ops1 ops2 comp eff .
  ( EffRow ops1
  , EffRow ops2
  , Effect eff
  )
  => Computation (Union ops1 ops2) comp eff
  -> Computation (Union ops2 ops1) comp eff
swapOps comp = castComputation comp Cast
