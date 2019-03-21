
module Control.Effect.Ops.Env
where

import Control.Effect.Base
import Control.Effect.Computation
import Control.Effect.Ops.LabeledEnv

data DefaultEnv where

type EnvEff e = LabeledEnvEff DefaultEnv e
type EnvOps e = LabeledEnvOps DefaultEnv e
type EnvCoOp e = LabeledEnvCoOp DefaultEnv e

type EnvConstraint e eff = (?envOps :: LabeledEnvOps DefaultEnv e eff)

instance EffOps (LabeledEnvEff DefaultEnv e) where
  type OpsConstraint (LabeledEnvEff DefaultEnv e) eff = EnvConstraint e eff

  withOps envOps comp = let ?envOps = envOps in comp
  captureOps = ?envOps

ask :: forall e eff . (Effect eff, EnvConstraint e eff) => eff e
ask = askLabel @DefaultEnv

mkEnvOps :: forall e eff . (Effect eff) => e -> EnvOps e eff
mkEnvOps = mkLabeledEnvOps @DefaultEnv

mkEnvHandler
  :: forall e eff .
  (Effect eff)
  => e
  -> BaseHandler (EnvEff e) eff
mkEnvHandler = mkLabeledEnvHandler
