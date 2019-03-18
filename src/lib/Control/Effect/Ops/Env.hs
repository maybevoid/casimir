{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Ops.Env
where

import Control.Effect.Base
import Control.Effect.Computation

data EnvEff e where

data EnvOps e eff = EnvOps {
  askOp :: eff e
}

data EnvCoOp e r =
  AskOp (e -> r)

type EnvConstraint e eff = (?envOps :: EnvOps e eff)

instance EffFunctor (EnvOps e) where
  effmap lifter envOps = EnvOps {
    askOp = lifter $ askOp envOps
  }

instance Functor (EnvCoOp e) where
  fmap f (AskOp cont) = AskOp $ fmap f cont

instance FreeOps (EnvEff e) where
  type Operation (EnvEff e) = EnvOps e
  type CoOperation (EnvEff e) = EnvCoOp e

  mkFreeOps liftCoOp = EnvOps {
    askOp = liftCoOp $ AskOp id
  }

instance EffOps (EnvEff e) where
  type OpsConstraint (EnvEff e) eff = (EnvConstraint e eff)

  withOps envOps comp = let ?envOps = envOps in comp

  captureOps = ?envOps

instance Normalizable (EnvEff e) where
  unionOps = UnionOps

ask :: forall e eff . (EnvConstraint e eff) => eff e
ask = askOp ?envOps

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
