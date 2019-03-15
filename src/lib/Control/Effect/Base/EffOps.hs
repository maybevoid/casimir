{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Effect.Base.EffOps
where

import GHC.Exts (Constraint)

import Control.Effect.Base.Effect
import Control.Effect.Base.FreeOps

class (FreeOps ops) => EffOps ops where
  type family OpsConstraint ops (eff :: * -> *)
    = (c :: Constraint) | c -> ops eff

  bindConstraint :: forall eff r .
    (Effect eff)
    => Operation ops eff
    -> (OpsConstraint ops eff => r)
    -> r

  captureOps :: forall eff .
    (Effect eff, OpsConstraint ops eff)
    => Operation ops eff
