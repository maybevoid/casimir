{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Ops.Env.Freer
where

import Control.Effect.Implicit.Ops.Env.Base
import Control.Effect.Implicit.Freer

data EnvCoOp e r where
  AskOp :: EnvCoOp e e

instance EffCoOp (EnvEff e) where
  type CoOperation (EnvEff e) = EnvCoOp e

instance FreeOps (EnvEff e) where
  mkFreeOps liftCoOp = EnvOps {
    askOp = liftCoOp $ AskOp
  }
