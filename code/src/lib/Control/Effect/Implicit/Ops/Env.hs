
module Control.Effect.Implicit.Ops.Env
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Free
import Control.Effect.Implicit.Freer
import Control.Effect.Implicit.Computation

data EnvEff e

data EnvOps e eff = EnvOps {
  askOp :: eff e
}

data EnvCoOp e r =
  AskOp (e -> r)

data EnvCoOp' e r where
  AskOp' :: EnvCoOp' e e

instance EffOps (EnvEff e) where
  type Operation (EnvEff e) = EnvOps e

instance EffCoOp (EnvEff e) where
  type CoOperation (EnvEff e) = EnvCoOp e

instance FreerEffCoOp (EnvEff e) where
  type FreerCoOp (EnvEff e) = EnvCoOp' e

instance EffFunctor (EnvOps e) where
  effmap lifter envOps = EnvOps {
    askOp = lifter $ askOp envOps
  }

instance Functor (EnvCoOp e) where
  fmap f (AskOp cont) = AskOp $ fmap f cont

instance FreeOps (EnvEff e) where
  mkFreeOps liftCoOp = EnvOps {
    askOp = liftCoOp $ AskOp id
  }

instance FreerOps (EnvEff e) where
  mkFreerOps liftCoOp = EnvOps {
    askOp = liftCoOp $ AskOp'
  }

instance ImplicitOps (EnvEff e) where
  type OpsConstraint (EnvEff e) eff =
    (?_Control_Effect_Implicit_Ops_Env_envOps :: EnvOps e eff)

  withOps envOps comp =
    let
      ?_Control_Effect_Implicit_Ops_Env_envOps =
        envOps in comp

  captureOps =
    ?_Control_Effect_Implicit_Ops_Env_envOps

ask :: forall e . Eff (EnvEff e) e
ask = askOp captureOps

withEnv
  :: forall r e eff
   . (Effect eff)
  => e
  -> ((OpsConstraint (EnvEff e) eff) => eff r)
  -> eff r
withEnv x cont = withOps (mkEnvOps x) cont

mkEnvOps :: forall e eff . (Effect eff) => e -> EnvOps e eff
mkEnvOps x = EnvOps {
  askOp = return x
}

mkEnvHandler
  :: forall e eff .
  (Effect eff)
  => e
  -> BaseOpsHandler (EnvEff e) eff
mkEnvHandler = baseOpsHandler . mkEnvOps
