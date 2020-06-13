module Casimir.Ops.Io.Base
where

import QuasiParam.Tag

import Casimir.Base

data IoTag
data IoEff

data IoOps m = IoOps {
  liftIoOp :: forall a . IO a -> m a
}

instance Effects IoEff where
  type Operations IoEff = IoOps

instance EffFunctor Lift IoOps where
  effmap (Lift lift) ops = IoOps {
    liftIoOp = lift . liftIoOp ops
  }

instance ImplicitOps IoEff where
  type OpsConstraint IoEff m =
    Param IoTag (IoOps m)

  withOps = withParam @IoTag
  captureOps = captureParam @IoTag

liftIo :: forall a . IO a -> Eff IoEff a
liftIo = liftIoOp captureOps

ioOps :: IoOps IO
ioOps = IoOps {
  liftIoOp = id
}
