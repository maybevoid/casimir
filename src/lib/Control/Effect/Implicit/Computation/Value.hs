
module Control.Effect.Implicit.Computation.Value
  ( Return (..)
  , GenericReturn
  , IdentityComputation
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

type IdentityComputation a = Computation NoEff (Return a) Identity

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
genericReturn comp = genericComputation $ Return comp

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