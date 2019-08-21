
module Control.Effect.Implicit.Ops.Env
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation

import qualified Control.Effect.Implicit.Free as Free
import qualified Control.Effect.Implicit.Freer as Freer

data EnvTag

data EnvOps e eff = EnvOps {
  askOp :: eff e
}

data EnvCoOp e r =
  AskOp (e -> r)

data EnvCoOp' e r where
  AskOp' :: EnvCoOp' e e

instance Free.EffCoOp (EnvOps e) where
  type CoOperation (EnvOps e) = EnvCoOp e

instance Freer.EffCoOp (EnvOps e) where
  type CoOperation (EnvOps e) = EnvCoOp' e

instance EffFunctor (EnvOps e) where
  effmap lifter envOps = EnvOps {
    askOp = lifter $ askOp envOps
  }

instance ImplicitOps (EnvOps e) where
  type OpsConstraint (EnvOps e) eff =
    TaggedParam EnvTag (EnvOps e eff)

  withOps = withTag @EnvTag
  captureOps = captureTag @EnvTag

instance Functor (EnvCoOp e) where
  fmap f (AskOp cont) = AskOp $ fmap f cont

instance Free.FreeOps (EnvOps e) where
  mkFreeOps liftCoOp = EnvOps {
    askOp = liftCoOp $ AskOp id
  }

instance Freer.FreeOps (EnvOps e) where
  mkFreeOps liftCoOp = EnvOps {
    askOp = liftCoOp $ AskOp'
  }

ask :: forall e . Eff (EnvOps e) e
ask = askOp captureOps

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
  -> BaseOpsHandler (EnvOps e) eff
mkEnvHandler = baseOpsHandler . mkEnvOps
