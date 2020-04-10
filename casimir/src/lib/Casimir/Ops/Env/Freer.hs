{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Ops.Env.Freer
where

import Casimir.Ops.Env.Base
import Casimir.Freer

data EnvCoOp e r where
  AskOp :: EnvCoOp e e

instance EffCoOp (EnvEff e) where
  type CoOperation (EnvEff e) = EnvCoOp e

instance FreeOps (EnvEff e) where
  mkFreeOps liftCoOp = EnvOps {
    askOp = liftCoOp $ AskOp
  }
