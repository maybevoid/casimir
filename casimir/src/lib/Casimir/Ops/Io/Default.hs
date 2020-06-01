module Casimir.Ops.Io.Default
where

import Data.QuasiParam.Tag

import Casimir.Base

import qualified Casimir.Ops.Io.Base as Base

data IoTag

type IoEff = TaggedEff IoTag Base.IoEff
type IoOps = TaggedOps IoTag Base.IoOps

pattern IoOps
  :: forall m
   . (forall a . IO a -> m a)
  -> IoOps m
pattern IoOps { liftIoOp } = LabeledOps (Base.IoOps liftIoOp)

liftIo :: forall a . IO a -> Eff IoEff a
liftIo = liftIoOp captureOps

ioOps :: IoOps IO
ioOps = IoOps {
  liftIoOp = id
}
