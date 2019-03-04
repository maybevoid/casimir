{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Effect.Base.EffFunctor
where

import Control.Effect.Base.Lift
import Control.Effect.Base.Effect

class EffFunctor (comp :: (* -> *) -> *) where
  -- type family WrapComp comp (f :: * -> *)
  --   = (c :: (* -> *) -> *) | c -> comp f

  effmap :: forall eff1 eff2 .
    (Effect eff1, Effect eff2)
    => LiftEff eff1 eff2
    -> comp eff1
    -> comp eff2

  -- wrapVal :: forall f eff .
  --   (Effect eff, EffFunctor (WrapComp comp f))
  --   => (forall a . a -> f a)
  --   -> comp eff
  --   -> WrapComp comp f eff