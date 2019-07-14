
module Control.Effect.Implicit.Ops.Env
where

import Data.Kind

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation

import qualified Control.Effect.Implicit.Free as Free
import qualified Control.Effect.Implicit.Freer as Freer

data EnvLabel

data EnvEff' e

data EnvOps' e eff = EnvOps {
  askOp :: eff e
}

data EnvCoOp e r =
  AskOp (e -> r)

data EnvCoOp' e r where
  AskOp' :: EnvCoOp' e e

instance EffOps (EnvEff' e) where
  type Operation (EnvEff' e) = EnvOps' e

instance Free.EffCoOp (EnvEff' e) where
  type CoOperation (EnvEff' e) = EnvCoOp e

instance Freer.EffCoOp (EnvEff' e) where
  type CoOperation (EnvEff' e) = EnvCoOp' e

instance EffFunctor (EnvOps' e) where
  effmap lifter envOps = EnvOps {
    askOp = lifter $ askOp envOps
  }

instance Functor (EnvCoOp e) where
  fmap f (AskOp cont) = AskOp $ fmap f cont

instance Free.FreeOps (EnvEff' e) where
  mkFreeOps liftCoOp = EnvOps {
    askOp = liftCoOp $ AskOp id
  }

instance Freer.FreeOps (EnvEff' e) where
  mkFreeOps liftCoOp = EnvOps {
    askOp = liftCoOp $ AskOp'
  }

type EnvEff e = LabeledEff EnvLabel (EnvEff' e)

type EnvOps e eff = LabeledOps EnvLabel (EnvEff' e) eff

ask :: forall e . Eff (EnvEff e) e
ask = askOp $ captureLabel @Type @EnvLabel

withEnv
  :: forall r e eff
   . (Effect eff)
  => e
  -> ((OpsConstraint (EnvEff e) eff) => eff r)
  -> eff r
withEnv x cont = withLabel @Type @EnvLabel @(EnvEff' e) @eff (unlabelOps $ mkEnvOps x) cont

mkEnvOps :: forall e eff . (Effect eff) => e -> EnvOps e eff
mkEnvOps x = LabeledOps $ EnvOps {
  askOp = return x
}

mkEnvHandler
  :: forall e eff .
  (Effect eff)
  => e
  -> BaseOpsHandler (EnvEff e) eff
mkEnvHandler = baseOpsHandler . mkEnvOps
