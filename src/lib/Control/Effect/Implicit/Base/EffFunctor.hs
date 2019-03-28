
module Control.Effect.Implicit.Base.EffFunctor
  ( EffFunctor (..)
  )
where

import Data.Kind

import Control.Effect.Implicit.Base.Effect

class EffFunctor (comp :: (Type -> Type) -> Type) where
  effmap :: forall eff1 eff2 .
    (Effect eff1, Effect eff2)
    => (forall x . eff1 x -> eff2 x)
    -> comp eff1
    -> comp eff2
