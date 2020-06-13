module Casimir.Ops.Io.Base
where

import QuasiParam.Tag

import Casimir.Base

data IoTag
data IoEff

data IoOps m = IoOps {
  liftIoOp :: forall a . IO a -> m a
}

instance Effect IoEff where
  type Operation IoEff = IoOps

instance EffFunctor Lift IoOps where
  effmap (Lift lift) ops = IoOps {
    liftIoOp = lift . liftIoOp ops
  }

instance HasLabel IoOps where
  type GetLabel IoOps = Tag IoTag

liftIo :: forall a . IO a -> Eff '[IoEff] a
liftIo = liftIoOp captureOp

ioOps :: IoOps IO
ioOps = IoOps {
  liftIoOp = id
}
