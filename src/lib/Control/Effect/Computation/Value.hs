
module Control.Effect.Computation.Value
where

import Data.Kind
import Control.Monad.Identity (Identity (..))

import Control.Effect.Base
import Control.Effect.Computation.Class

newtype PureVal a (eff :: Type -> Type) = PureVal {
  pureVal :: a
}

newtype Return a eff = Return {
  returnVal :: eff a
}

instance EffFunctor (Return a) where
  effmap lifter (Return mx) = Return $ lifter mx

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
      => LiftEff eff1 eff2
      -> eff2 a)
  -> ReturnComputation ops a eff1
returnComputation comp1 = Computation $
  \ lift12 ops ->
    withOps ops $ Return $ comp1 lift12

{-# INLINE genericComputation #-}
genericComputation
  :: forall ops comp .
  (EffOps ops)
  => (forall eff .
      (Effect eff, OpsConstraint ops eff)
      => comp eff)
  -> (forall eff .Computation ops comp eff)
genericComputation comp = Computation $
  \ _ ops -> withOps ops comp

{-# INLINE genericReturn #-}
genericReturn
  :: forall ops a .
  (EffOps ops)
  => (forall eff .
      (Effect eff, OpsConstraint ops eff)
      => eff a)
  -> GenericReturn ops a
genericReturn comp = Computation $
  \ _ ops -> Return $ withOps ops comp

runIdentityComp :: forall a . IdentityComputation a -> a
runIdentityComp comp = runIdentity $ returnVal $ runComp comp idLift NoOp

execComp
  :: forall ops eff a .
  ( EffOps ops
  , Effect eff
  , OpsConstraint ops eff
  )
  => Computation ops (Return a) eff
  -> eff a
execComp comp = returnVal $ runComp comp idLift captureOps