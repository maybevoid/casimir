{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Effect.Base.EffFunctor
where

import GHC.Exts (Constraint)

import Control.Effect.Base.Lift
import Control.Effect.Base.Effect

class EffFunctor (comp :: (* -> *) -> *) where
  type family WrapComp comp (f :: * -> *)
    = (c :: (* -> *) -> *) | c -> comp f

  type family WrapConstraint comp (f :: * -> *)
    :: Constraint

  effmap :: forall eff1 eff2 .
    (Effect eff1, Effect eff2)
    => LiftEff eff1 eff2
    -> comp eff1
    -> comp eff2

  wrapEff :: forall f eff1 eff2 .
    ( Effect eff1
    , Effect eff2
    , EffFunctor (WrapComp comp f)
    , WrapConstraint comp f
    )
    => (forall a . eff1 a -> eff2 (f a))
    -> comp eff1
    -> WrapComp comp f eff2