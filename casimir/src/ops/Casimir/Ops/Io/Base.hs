module Casimir.Ops.Io.Base
where

import QuasiParam.Tag

import Casimir.Base

data IoTag

data IoOps m = IoOps {
  liftIoOp :: forall a . IO a -> m a
}

instance EffFunctor Lift IoOps where
  effmap (Lift lift) ops = IoOps {
    liftIoOp = lift . liftIoOp ops
  }

instance HasLabel IoOps where
  type GetLabel IoOps = Tag IoTag

liftIo :: forall a . IO a -> Eff '[IoOps] a
liftIo = liftIoOp captureOp

ioOps :: IoOps IO
ioOps = IoOps {
  liftIoOp = id
}
