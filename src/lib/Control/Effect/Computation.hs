
module Control.Effect.Computation
  ( Pure (..)
  , Return (..)
  , PureComputation
  , IdentityComputation
  , EffectfulComputation
  , liftComputation
  , pureComputation
  , runIdentityComp
  , effectfulComputation
  )
where

import Control.Monad.Identity (Identity (..))

import Control.Effect.Base
  ( NoEff
  , Effect
  , LiftEff
  , joinLift
  , EffOps (..)
  , Computation (..)
  )

newtype Pure a (eff :: * -> *) = Pure {
  pureVal :: a
}

newtype Return a eff = Return {
  returnVal :: eff a
}

type PureComputation a =
  forall eff . Computation NoEff (Pure a) eff

type EffectfulComputation ops a eff =
  Computation ops (Return a) eff

type IdentityComputation a = EffectfulComputation NoEff a Identity

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

pureComputation :: forall a . a -> PureComputation a
pureComputation x = Computation comp
  where
    comp :: forall eff1 eff2 .
      (Effect eff1, Effect eff2)
      => LiftEff eff1 eff2
      -> Pure a eff2
    comp _ = Pure x

effectfulComputation
  :: forall ops a eff .
  (EffOps ops)
  => (forall eff' . (Effect eff', EffConstraint ops eff') => eff' a)
  -> EffectfulComputation ops a eff
effectfulComputation comp1 = Computation comp2
  where
    comp2 :: forall eff1 eff2 .
      (Effect eff1, Effect eff2, EffConstraint ops eff2)
      => LiftEff eff1 eff2
      -> Return a eff2
    comp2 _ = Return comp1

runIdentityComp :: forall a . IdentityComputation a -> a
runIdentityComp comp = runIdentity $ returnVal $ runComp comp id
