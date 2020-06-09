
module Casimir.Ops.Env.Base
where

import QuasiParam.Tag

import Casimir.Base
import Casimir.Computation

data EnvTag
data EnvEff e

data EnvOps e m = EnvOps {
  askOp :: m e
}

instance Effects (EnvEff e) where
  type Operations' (EnvEff e) = EnvOps e

instance EffFunctor Lift (EnvOps e) where
  effmap (Lift lift) envOps = EnvOps {
    askOp = lift $ askOp envOps
  }

instance ImplicitOps (EnvEff e) where
  type OpsConstraint (EnvEff e) m =
    Param EnvTag (EnvOps e m)

  withOps = withParam @EnvTag
  captureOps = captureParam @EnvTag

ask :: forall e . Eff (EnvEff e) e
ask = askOp captureOps

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

mkEnvHandler
  :: forall e m .
  (Monad m)
  => e
  -> BaseOpsHandler NoEff (EnvEff e) m
mkEnvHandler = baseOpsHandler . mkEnvOps
