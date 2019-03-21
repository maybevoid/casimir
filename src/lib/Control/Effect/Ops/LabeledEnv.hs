{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Ops.LabeledEnv
where

import Data.Kind
import Control.Effect.Base
import Control.Effect.Computation

data LabeledEnvEff (l :: k) (e :: Type) where

data LabeledEnvOps l e eff = LabeledEnvOps {
  askOp :: eff e
}

data LabeledEnvCoOp l e r =
  AskOp (e -> r)
  deriving (Functor)

instance EffFunctor (LabeledEnvOps l e) where
  effmap lifter ops = LabeledEnvOps {
    askOp = lifter $ askOp ops
  }

instance FreeOps (LabeledEnvEff l e) where
  type Operation (LabeledEnvEff l e) = LabeledEnvOps l e
  type CoOperation (LabeledEnvEff l e) = LabeledEnvCoOp l e

  mkFreeOps liftCoOp = LabeledEnvOps {
    askOp = liftCoOp $ AskOp id
  }

instance (EffOps (LabeledEnvEff l e)) => Normalizable (LabeledEnvEff l e) where
  unionOps = UnionOps

askLabel
  :: forall l e eff
   . ( Effect eff
     , EffOps (LabeledEnvEff l e)
     , OpsConstraint (LabeledEnvEff l e) eff
     )
  => eff e
askLabel = askOp $ captureOps @(LabeledEnvEff l e)

mkLabeledEnvOps :: forall l e eff . (Effect eff) => e -> LabeledEnvOps l e eff
mkLabeledEnvOps x = LabeledEnvOps {
  askOp = return x
}

mkLabeledEnvHandler
  :: forall l e eff .
  (Effect eff, EffOps (LabeledEnvEff l e))
  => e
  -> BaseHandler (LabeledEnvEff l e) eff
mkLabeledEnvHandler = baseHandler . mkLabeledEnvOps
