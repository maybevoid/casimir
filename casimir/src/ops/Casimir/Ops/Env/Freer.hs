{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Ops.Env.Freer
where

import Casimir.Ops.Env.Base
import Casimir.Freer

data EnvCoOp e r where
  AskOp :: EnvCoOp e e

instance FreeOps (EnvOps e) where
  type CoOperation (EnvOps e) = EnvCoOp e

  mkFreeOps liftCoOp = EnvOps {
    askOp = liftCoOp $ AskOp
  }
