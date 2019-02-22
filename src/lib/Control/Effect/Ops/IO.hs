
module Control.Effect.Ops.IO where

import Control.Effect.Class

data IoOps eff = IoOps {
  liftIoOp :: forall a . IO a -> eff a
}

type IoEff eff = (?ioOps :: IoOps eff)

instance EffFunctor IoOps where
  effmap f ioOps = IoOps {
    liftIoOp = liftEff f . liftIoOp ioOps
  }

instance EffRow IoOps where
  type EffConstraint IoOps eff = (IoEff eff)

  bindConstraint ioOps comp = let ?ioOps = ioOps in comp

liftIO :: forall a eff .
  (IoEff eff)
  => IO a -> eff a
liftIO = liftIoOp ?ioOps