
module Control.Effect.Computation.Value
where

import Control.Monad.Identity (Identity (..))

import Control.Effect.Base
import Control.Effect.Computation.Class

newtype PureVal a (eff :: * -> *) = PureVal {
  pureVal :: a
}

newtype Return a eff = Return {
  returnVal :: eff a
}

instance EffFunctor (Return a) where
  effmap liftEff (Return mx) = Return $ liftEff mx

type PureComputation a =
  forall eff . Computation NoEff (PureVal a) eff

type ReturnComputation ops a eff =
  Computation ops (Return a) eff

type GenericReturn ops a =
  forall eff . (Effect eff) => ReturnComputation ops a eff

type GenericComputation ops comp =
  forall eff . (Effect eff) => Computation ops comp eff

type IdentityComputation a = ReturnComputation NoEff a Identity

pureComputation :: forall a . a -> PureComputation a
pureComputation x = Computation $ \ _ _ -> PureVal x

returnComputation
  :: forall ops a eff1 .
  ( Effect eff1
  , EffOps ops
  )
  => (forall eff2 .
      (Effect eff2, OpsConstraint ops eff2)
      => eff1 ~> eff2
      -> eff2 a)
  -> ReturnComputation ops a eff1
returnComputation comp1 = Computation $
  \ liftEff ops ->
    bindConstraint ops $ Return $ comp1 liftEff

genericComputation
  :: forall ops comp .
  (EffOps ops)
  => (forall eff .
      (Effect eff, OpsConstraint ops eff)
      => comp eff)
  -> (forall eff .Computation ops comp eff)
genericComputation comp = Computation $
  \ _ ops -> bindConstraint ops comp

genericReturn
  :: forall ops a .
  (EffOps ops)
  => (forall eff .
      (Effect eff, OpsConstraint ops eff)
      => eff a)
  -> GenericReturn ops a
genericReturn comp = Computation $
  \ _ ops -> Return $ bindConstraint ops comp

runIdentityComp :: forall a . IdentityComputation a -> a
runIdentityComp comp = runIdentity $ returnVal $ runComp comp id NoOp

execComp
  :: forall ops eff a .
  ( EffOps ops
  , Effect eff
  , OpsConstraint ops eff
  )
  => Computation ops (Return a) eff
  -> eff a
execComp comp = returnVal $ runComp comp id captureOps