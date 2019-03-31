
module Control.Effect.Implicit.Computation.Value
  ( Return (..)
  , GenericReturn
  , GenericComputation
  , IdentityComputation
  , returnComputation
  , genericComputation
  , genericReturn
  , runIdentityComp
  , execComp
  )
where

import Control.Monad.Identity (Identity (..))

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation.Computation

newtype Return a eff = Return {
  returnVal :: eff a
}

instance EffFunctor (Return a) where
  effmap lifter (Return mx) = Return $ lifter mx

type GenericReturn ops a =
  forall eff . (Effect eff) => Computation ops (Return a) eff

type GenericComputation ops comp =
  forall eff . (Effect eff) => Computation ops comp eff

type IdentityComputation a = Computation NoEff (Return a) Identity

returnComputation
  :: forall ops a eff1 .
  ( Effect eff1
  , ImplicitOps ops
  )
  => (forall eff2 .
      (EffConstraint ops eff2)
      => LiftEff eff1 eff2
      -> eff2 a)
  -> Computation ops (Return a) eff1
returnComputation comp1 = Computation $
  \ lift12 ops ->
    withOps ops $ Return $ comp1 lift12

{-# INLINE genericComputation #-}
genericComputation
  :: forall ops comp .
  (ImplicitOps ops)
  => (forall eff .
      (EffConstraint ops eff)
      => comp eff)
  -> (forall eff . (Effect eff) => Computation ops comp eff)
genericComputation comp = Computation $
  \ _ ops -> withOps ops comp

{-# INLINE genericReturn #-}
genericReturn
  :: forall ops a .
  (ImplicitOps ops)
  => (forall eff .
      (EffConstraint ops eff)
      => eff a)
  -> GenericReturn ops a
genericReturn comp = Computation $
  \ _ ops -> Return $ withOps ops comp

runIdentityComp :: forall a . IdentityComputation a -> a
runIdentityComp comp = runIdentity $ returnVal $ runComp comp idLift NoOp

execComp
  :: forall ops eff a .
  ( ImplicitOps ops
  , EffConstraint ops eff
  )
  => Computation ops (Return a) eff
  -> eff a
execComp comp = returnVal $ runComp comp idLift captureOps