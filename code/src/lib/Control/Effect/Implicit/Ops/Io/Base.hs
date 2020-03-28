module Control.Effect.Implicit.Ops.Io.Base
where

import Control.Effect.Implicit.Base

data IoTag
data IoEff

data IoOps eff = IoOps {
  liftIoOp :: forall a . IO a -> eff a
}

instance EffOps IoEff where
  type Operation IoEff = IoOps

instance EffFunctor IoOps where
  effmap lifter ops = IoOps {
    liftIoOp = lifter . liftIoOp ops
  }

instance ImplicitOps IoEff where
  type OpsConstraint IoEff eff =
    TaggedParam IoTag (IoOps eff)

  withOps = withTag @IoTag
  captureOps = captureTag @IoTag

liftIo :: forall a . IO a -> Eff IoEff a
liftIo = liftIoOp captureOps

ioOps :: IoOps IO
ioOps = IoOps {
  liftIoOp = id
}
