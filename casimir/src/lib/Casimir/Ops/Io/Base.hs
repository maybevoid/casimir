module Casimir.Ops.Io.Base
where

import Data.QuasiParam.Tag

import Casimir.Base

data IoTag
data IoEff

data IoOps eff = IoOps {
  liftIoOp :: forall a . IO a -> eff a
}

instance EffOps IoEff where
  type Operation IoEff = IoOps

instance EffFunctor Lift IoOps where
  effmap (Lift lift) ops = IoOps {
    liftIoOp = lift . liftIoOp ops
  }

instance ImplicitOps IoEff where
  type OpsConstraint IoEff eff =
    Param IoTag (IoOps eff)

  withOps = withParam @IoTag
  captureOps = captureParam @IoTag

liftIo :: forall a . IO a -> Eff IoEff a
liftIo = liftIoOp captureOps

ioOps :: IoOps IO
ioOps = IoOps {
  liftIoOp = id
}
