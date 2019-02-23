{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Effect.Computation where

import Control.Effect.Class
import Control.Effect.Ops.NoOp

newtype Pure a (eff :: * -> *) = Pure {
  pureVal :: a
}

newtype Return a eff = Return {
  returnVal :: eff a
}

type PureValue a =
  forall eff . Computation NoOp (Pure a) eff

type EffectfulValue ops a eff =
  Computation ops (Return a) eff

type EvaluatedValue a = EffectfulValue NoOp a Identity

pureValue :: forall a . a -> PureValue a
pureValue x = Computation comp
  where
    comp :: forall eff1 eff2 .
      (Effect eff1, Effect eff2)
      => LiftEff eff1 eff2
      -> Pure a eff2
    comp _ = Pure x

effectfulValue
  :: forall ops a eff .
  (EffOps ops)
  => (forall eff' . (Effect eff', EffConstraint ops eff') => eff' a)
  -> EffectfulValue ops a eff
effectfulValue comp1 = Computation comp2
  where
    comp2 :: forall eff1 eff2 .
      (Effect eff1, Effect eff2, EffConstraint ops eff2)
      => LiftEff eff1 eff2
      -> Return a eff2
    comp2 _ = Return comp1

extractReturn :: forall a . Return a Identity -> a
extractReturn ret = runIdentity $ returnVal ret

extractValue :: forall a . EvaluatedValue a -> a
extractValue comp = extractReturn $ runComp comp idLift

liftComputation :: forall ops comp eff1 eff2 .
  (EffOps ops, Effect eff1, Effect eff2)
  => LiftEff eff1 eff2
  -> Computation ops comp eff1
  -> Computation ops comp eff2
liftComputation lift12 comp1 = Computation comp2
  where
    comp2 :: forall eff3 .
      (Effect eff3)
      => LiftEff eff2 eff3
      -> (EffConstraint ops eff3 => comp eff3)
    comp2 lift23 = runComp comp1 $ joinLift lift12 lift23
