{-# LANGUAGE UndecidableInstances #-}

module Casimir.Base.EffFunctor
  ( EffFunctor (..)
  )
where

import Data.Kind

import Casimir.Base.Lift
import Casimir.Base.Effect

class EffFunctor
  (lift :: (Type -> Type) -> (Type -> Type) -> Type)
  (comp :: (Type -> Type) -> Type)
  where
    effmap
      :: forall eff1 eff2
      .  (Effect eff1, Effect eff2)
      => lift eff1 eff2
      -> comp eff1
      -> comp eff2

instance {-# OVERLAPPABLE #-}
  ( EffFunctor Lift comp
  )
  => EffFunctor HigherLift comp where
    effmap
      :: forall eff1 eff2
       . (Effect eff1, Effect eff2)
      => HigherLift eff1 eff2
      -> comp eff1
      -> comp eff2
    effmap (HigherLift lift _) comp =
      effmap (Lift lift) comp
