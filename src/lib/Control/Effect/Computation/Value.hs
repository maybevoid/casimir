
module Control.Effect.Computation.Value
where

import Control.Monad.Identity (Identity (..))

import Control.Effect.Base
import Control.Effect.Computation.Class

newtype Pure a (eff :: * -> *) = Pure {
  pureVal :: a
}

newtype Return a eff = Return {
  returnVal :: eff a
}

instance EffFunctor (Return a) where
  type WrapComp (Return a) f = (Return (f a))
  type WrapConstraint (Return a) f = ()

  effmap liftEff (Return mx) = Return $ liftEff mx

  wrapEff wrap (Return mx) = Return $ wrap mx

type PureComputation a =
  forall eff . Computation NoEff (Pure a) eff

type EffectfulComputation ops a eff =
  Computation ops (Return a) eff

type GenericComputation ops a =
  forall eff .  EffectfulComputation ops a eff

type IdentityComputation a = EffectfulComputation NoEff a Identity

pureComputation :: forall a . a -> PureComputation a
pureComputation x = Computation $ \ _ _ -> Pure x

effectfulComputation
  :: forall ops a eff1 .
  ( Effect eff1
  , EffOps ops
  )
  => (forall eff2 .
      (Effect eff2, OpsConstraint ops eff2)
      => LiftEff eff1 eff2
      -> eff2 a)
  -> EffectfulComputation ops a eff1
effectfulComputation comp1 = Computation $
  \ liftEff ops ->
    bindConstraint ops $ Return $ comp1 liftEff

genericComputation
  :: forall ops a .
  ( EffOps ops )
  => (forall eff .
      (Effect eff, OpsConstraint ops eff)
      => eff a)
  -> GenericComputation ops a
genericComputation comp = Computation $
  \ _ ops -> bindConstraint ops $ Return comp

runIdentityComp :: forall a . IdentityComputation a -> a
runIdentityComp comp = runIdentity $ returnVal $ runComp comp id NoOp
