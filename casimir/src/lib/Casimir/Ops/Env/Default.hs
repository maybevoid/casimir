
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
  :: forall e m
   . m e
  -> EnvOps e m
pattern EnvOps { askOp }
  = LabeledOps (Base.EnvOps askOp)

ask :: forall e . Eff (EnvEff e) e
ask = ops
 where
  (EnvOps ops) = captureOps

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
