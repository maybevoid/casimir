
module Control.Effect.Ops.State where

import Control.Effect.Class

data StateOps a eff = StateOps {
  getOp :: eff a,
  putOp :: a -> eff ()
}

type StateEff a eff = (?stateOps :: StateOps a eff)

instance EffFunctor (StateOps a) where
  effmap f stateOps = StateOps {
    getOp = liftEff f $ getOp stateOps,
    putOp = liftEff f . putOp stateOps
  }

instance EffRow (StateOps a) where
  type EffConstraint (StateOps a) eff = StateEff a eff

  bindConstraint stateOps comp = let ?stateOps = stateOps in comp

get :: forall a eff .
  (StateEff a eff)
  => eff a
get = getOp ?stateOps


put :: forall a eff .
  (StateEff a eff)
  => a
  -> eff ()
put = putOp ?stateOps