module Casimir.Ops.Env.Effect where

import Casimir.Base

data EnvTag

data EnvOps e m = EnvOps
  { askOp :: m e
  }

instance HasLabel (EnvOps e) where
  type GetLabel (EnvOps e) = EnvTag

ask :: forall e . Eff1 (EnvOps e) e
ask = withCaptureOps askOp

instance EffFunctor Lift (EnvOps e) where
  effmap (Lift lift) (EnvOps askOp') =
    EnvOps $ lift askOp'
