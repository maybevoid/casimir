{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Effect.Base.EffOps
where

import Data.Kind

import Control.Effect.Base.Effect
import Control.Effect.Base.FreeOps

class
  (FreeOps ops)
  => EffOps ops where
    type family OpsConstraint ops (eff :: Type -> Type)
      = (c :: Constraint) | c -> ops eff

    withOps :: forall eff r .
      (Effect eff)
      => Operation ops eff
      -> (OpsConstraint ops eff => r)
      -> r

    captureOps :: forall eff .
      (Effect eff, OpsConstraint ops eff)
      => Operation ops eff
