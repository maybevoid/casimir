{-# LANGUAGE DeriveFunctor #-}

module Control.Effect.Ops.Decide
where

import Control.Effect.Base
import Control.Effect.Dynamic

data DecideEff s where

data DecideOps s eff = DecideOps {
  decideOp :: eff s
}

data DecideModel s a = DecideOp (s -> a)
  deriving (Functor)

type DecideConstraint s eff = (?decideOps :: DecideOps s eff)

instance EffFunctor (DecideOps s) where
  effmap liftEff decideOps = DecideOps {
    decideOp = liftEff $ decideOp decideOps
  }

instance FreeOps (DecideEff s) where
  type Operation (DecideEff s) = DecideOps s
  type CoOperation (DecideEff s) = DecideModel s

  mkFreeOps liftCoOps = DecideOps {
    decideOp = liftCoOps $ DecideOp id
  }

instance EffOps (DecideEff s) where
  type OpsConstraint (DecideEff s) eff = DecideConstraint s eff

  bindConstraint decideOps comp = let ?decideOps = decideOps in comp

  captureOps = ?decideOps

instance Normalizable (DecideEff s) where
  unionOps = UnionOps

decide :: forall a eff .
  (DecideConstraint a eff)
  => eff a
decide = decideOp ?decideOps

dynamicDecideOps
  :: forall s eff .
  (Effect eff)
  => DecideOps s (DynamicMonad (DecideEff s) eff)
dynamicDecideOps = DecideOps {
  decideOp = liftOps $ (DecideOp return)
}