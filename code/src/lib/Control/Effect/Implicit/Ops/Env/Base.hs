
module Control.Effect.Implicit.Ops.Env.Base
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation

data EnvTag
data EnvEff e

data EnvOps e eff = EnvOps {
  askOp :: eff e
}

instance EffOps (EnvEff e) where
  type Operation (EnvEff e) = EnvOps e

instance EffFunctor (EnvOps e) where
  effmap lifter envOps = EnvOps {
    askOp = lifter $ askOp envOps
  }

instance ImplicitOps (EnvEff e) where
  type OpsConstraint (EnvEff e) eff =
    TaggedParam EnvTag (EnvOps e eff)

  withOps = withTag @EnvTag
  captureOps = captureTag @EnvTag

ask :: forall e . Eff (EnvEff e) e
ask = askOp captureOps

withEnv
  :: forall r e eff
   . (Effect eff)
  => e
  -> (EnvOps e eff -> eff r)
  -> eff r
withEnv x cont = cont (mkEnvOps x)

mkEnvOps :: forall e eff . (Effect eff) => e -> EnvOps e eff
mkEnvOps x = EnvOps {
  askOp = return x
}

mkEnvHandler
  :: forall e eff .
  (Effect eff)
  => e
  -> BaseOpsHandler NoEff (EnvEff e) eff
mkEnvHandler = baseOpsHandler . mkEnvOps
