
module Casimir.Ops.Env.Default
where

import Data.QuasiParam.Tag

import Casimir.Base
import Casimir.Computation

import qualified Casimir.Ops.Env.Base as Base

data EnvTag

type EnvEff e = TaggedEff EnvTag (Base.EnvEff e)
type EnvOps e = TaggedOps EnvTag (Base.EnvOps e)

pattern EnvOps
  :: forall e eff
   . eff e
  -> EnvOps e eff
pattern EnvOps { askOp }
  = LabeledOps (Base.EnvOps askOp)

ask :: forall e . Eff (EnvEff e) e
ask = ops
 where
  (EnvOps ops) = captureOps

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
