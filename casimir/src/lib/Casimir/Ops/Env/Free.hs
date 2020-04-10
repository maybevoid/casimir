{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Ops.Env.Free
where

import Casimir.Ops.Env.Base
import Casimir.Free

data EnvCoOp e r =
  AskOp (e -> r)

instance Functor (EnvCoOp e) where
  fmap f (AskOp cont) = AskOp $ fmap f cont

instance EffCoOp (EnvEff e) where
  type CoOperation (EnvEff e) = EnvCoOp e

instance FreeOps (EnvEff e) where
  mkFreeOps liftCoOp = EnvOps {
    askOp = liftCoOp $ AskOp id
  }
