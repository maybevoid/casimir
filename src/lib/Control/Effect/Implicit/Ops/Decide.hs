
module Control.Effect.Implicit.Ops.Decide
where

import Control.Effect.Implicit.Base

data DecideEff s

data DecideOps s eff = DecideOps {
  decideOp :: eff s
}

data DecideCoOp s a = DecideOp (s -> a)
  deriving (Functor)

type DecideConstraint s eff = (?decideOps :: DecideOps s eff)

instance EffFunctor (DecideOps s) where
  effmap lifter ops = DecideOps {
    decideOp = lifter $ decideOp ops
  }

instance FreeOps (DecideEff s) where
  type Operation (DecideEff s) = DecideOps s
  type CoOperation (DecideEff s) = DecideCoOp s

  mkFreeOps liftCoOp = DecideOps {
    decideOp = liftCoOp $ DecideOp id
  }

instance EffOps (DecideEff s) where
  type OpsConstraint (DecideEff s) eff = DecideConstraint s eff

  withOps decideOps comp = let ?decideOps = decideOps in comp

  captureOps = ?decideOps

decide :: forall a eff .
  (DecideConstraint a eff)
  => eff a
decide = decideOp ?decideOps
