module Control.Effect.Old.Computation
where

import Control.Monad.Identity (Identity (..))

import Control.Effect.Base
  ( NoEff
  , Effect
  , LiftEff
  , joinLift
  , EffOps (..)
  , FreeEff (..)
  , EffFunctor (..)
  )

data Computation ops comp eff = Computation {
  runComp :: forall eff' .
    (EffOps ops, Effect eff, Effect eff')
    => LiftEff eff eff'
    -> ((OpsConstraint ops eff') => comp eff')
}

data Handler ops handler eff1 eff2
  = Handler
    (LiftEff eff2 eff1)
    (Computation ops (Operation handler) eff1)

newtype Pure a (eff :: * -> *) = Pure {
  pureVal :: a
}

newtype Return a eff = Return {
  returnVal :: eff a
}

instance EffFunctor (Return a) where
  -- type WrapComp (Return a) f = (Return (f a))

  effmap liftEff (Return mx) = Return $ liftEff mx

  -- wrapVal wrap (Return mx) = Return $ fmap wrap mx

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
      -> (OpsConstraint ops eff3 => comp eff3)
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
  => (forall eff' . (Effect eff', OpsConstraint ops eff') => eff' a)
  -> EffectfulComputation ops a eff
effectfulComputation comp1 = Computation comp2
  where
    comp2 :: forall eff1 eff2 .
      (Effect eff1, Effect eff2, OpsConstraint ops eff2)
      => LiftEff eff1 eff2
      -> Return a eff2
    comp2 _ = Return comp1

runIdentityComp :: forall a . IdentityComputation a -> a
runIdentityComp comp = runIdentity $ returnVal $ runComp comp id
