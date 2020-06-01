module Casimir.Ops.Io.Base
where

import Data.QuasiParam.Tag

import Casimir.Base

data IoEff

data IoOps m = IoOps
  { liftIoOp :: forall a . IO a -> m a
  }

instance EffOps IoEff where
  type Operation IoEff = IoOps

instance EffFunctor Lift IoOps where
  mmap (Lift lift) ops = IoOps
    { liftIoOp = lift . liftIoOp ops
    }

ioOps :: IoOps IO
ioOps = IoOps
  { liftIoOp = id }
