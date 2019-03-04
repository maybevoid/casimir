
module Control.Effect.Ops.Io
  ( IoOps (..)
  , IoModel (..)
  , IoEff
  , liftIo
  , freeIoOps
  )
where

import Control.Natural (type (~>))
import Control.Monad.Trans.Free (FreeT, liftF)

import Control.Effect.Base
  ( Effect
  , EffFunctor (..)
  , FreeEff (..)
  , EffOps (..)
  )

data IoOps eff = IoOps {
  liftIoOp :: forall a . IO a -> eff a
}

data IoModel a = LiftIO (IO a)

type IoEff eff = (?ioOps :: IoOps eff)

instance Functor IoModel where
  fmap f (LiftIO io) = LiftIO $ fmap f io

instance EffFunctor IoOps where
  effmap liftEff ioOps = IoOps {
    liftIoOp = liftEff . liftIoOp ioOps
  }

instance FreeEff IoOps where
  type FreeModel IoOps = IoModel

  freeModel = freeIoOps

instance EffOps IoOps where
  type EffConstraint IoOps eff = (IoEff eff)

  bindConstraint ioOps comp = let ?ioOps = ioOps in comp

liftIo :: forall a eff .
  (IoEff eff)
  => IO a -> eff a
liftIo = liftIoOp ?ioOps

freeIoOps
  :: forall f eff .
  (Functor f, Effect eff)
  => IoModel ~> f
  -> IoOps (FreeT f eff)
freeIoOps liftModel = IoOps {
  liftIoOp = \io -> liftF $ liftModel $ LiftIO io
}