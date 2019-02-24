
module Control.Effect.Ops.IO where

import Control.Natural
import Control.Monad.Free

import Control.Effect.Class

data IoOps eff = IoOps {
  liftIoOp :: forall a . IO a -> eff a
}

data IoModel a = LiftIO (IO a)

type IoEff eff = (?ioOps :: IoOps eff)

instance Functor IoModel where
  fmap f (LiftIO io) = LiftIO $ fmap f io

instance EffFunctor IoOps where
  effmap f ioOps = IoOps {
    liftIoOp = liftEff f . liftIoOp ioOps
  }

instance FreeEff IoOps where
  type FreeModel IoOps = IoModel

  freeModel = freeIoOps

instance EffOps IoOps where
  type EffConstraint IoOps eff = (IoEff eff)

  bindConstraint ioOps comp = let ?ioOps = ioOps in comp

liftIO :: forall a eff .
  (IoEff eff)
  => IO a -> eff a
liftIO = liftIoOp ?ioOps

freeIoOps
  :: forall f .
  (Functor f)
  => IoModel ~> f
  -> IoOps (Free f)
freeIoOps liftModel = IoOps {
  liftIoOp = \io -> liftF $ liftModel $ LiftIO io
}