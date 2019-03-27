
module Control.Effect.Implicit.Base.EffOps
  ( EffOps (..)
  , EffConstraint
  )
where

import Data.Kind

import Control.Effect.Implicit.Base.Effect
import Control.Effect.Implicit.Base.FreeOps

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

type EffConstraint ops eff = (Effect eff, OpsConstraint ops eff)