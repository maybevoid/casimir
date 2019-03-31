
module Control.Effect.Implicit.Ops.Env
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation

data EnvEff e

data EnvOps e eff = EnvOps {
  askOp :: eff e
}

data EnvCoOp e r =
  AskOp (e -> r)

instance EffOps (EnvEff e) where
  type Operation (EnvEff e) = EnvOps e

instance EffCoOp (EnvEff e) where
  type CoOperation (EnvEff e) = EnvCoOp e

type EnvConstraint e eff = (?envOps :: EnvOps e eff)

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

instance ImplicitOps (EnvEff e) where
  type OpsConstraint (EnvEff e) eff = (EnvConstraint e eff)

  withOps envOps comp = let ?envOps = envOps in comp

  captureOps = ?envOps

ask :: forall e eff . (EnvConstraint e eff) => eff e
ask = askOp ?envOps

withEnv
  :: forall r e eff
   . (Effect eff)
  => e
  -> ((EnvConstraint e eff) => eff r)
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
  -> BaseHandler (EnvEff e) eff
mkEnvHandler = baseHandler . mkEnvOps