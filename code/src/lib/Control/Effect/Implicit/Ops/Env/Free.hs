{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Ops.Env.Free
where

import Control.Effect.Implicit.Ops.Env.Base
import Control.Effect.Implicit.Free

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
