
module Control.Effect.Ops.Env where

import Control.Effect.Class

data EnvOps a eff = EnvOps {
  askOp :: eff a
}

type EnvEff a eff = (?envOps :: EnvOps a eff)

instance EffFunctor (EnvOps a) where
  effmap f envOps = EnvOps {
    askOp = liftEff f $ askOp envOps
  }

instance EffOps (EnvOps a) where
  type EffConstraint (EnvOps a) eff = (EnvEff a eff)

  bindConstraint envOps comp = let ?envOps = envOps in comp

ask :: forall a eff . (EnvEff a eff) => eff a
ask = askOp ?envOps
