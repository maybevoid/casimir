
module Casimir.Ops.Env.Base
where

import QuasiParam.Tag

import Casimir.Base

data EnvTag
data EnvEff e

data EnvOps e m = EnvOps {
  askOp :: m e
}

instance Effect (EnvEff e) where
  type Operation (EnvEff e) = EnvOps e

instance EffFunctor Lift (EnvOps e) where
  effmap (Lift lift) envOps = EnvOps {
    askOp = lift $ askOp envOps
  }

instance HasLabel (EnvOps e) where
  type GetLabel (EnvOps e) = Tag EnvTag

ask :: forall e . Eff '[EnvEff e] e
ask = askOp $ captureOp

withEnv
  :: forall r e m
   . (Monad m)
  => e
  -> (EnvOps e m -> m r)
  -> m r
withEnv x cont = cont (mkEnvOps x)

mkEnvOps :: forall e m . (Monad m) => e -> EnvOps e m
mkEnvOps x = EnvOps {
  askOp = return x
}
