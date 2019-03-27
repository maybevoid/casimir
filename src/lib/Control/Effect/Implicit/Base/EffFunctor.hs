{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Effect.Implicit.Base.EffFunctor
where

import Data.Kind
import Control.Effect.Implicit.Base.Effect

class EffFunctor (comp :: (Type -> Type) -> Type) where
  effmap :: forall eff1 eff2 .
    (Effect eff1, Effect eff2)
    => eff1 ~> eff2
    -> comp eff1
    -> comp eff2
