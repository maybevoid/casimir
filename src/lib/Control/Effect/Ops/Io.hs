
module Control.Effect.Ops.Io
  ( IoEff
  , IoOps (..)
  , IoModel (..)
  , IoConstraint
  , liftIo
  , freeIoOps
  , ioOps
  , ioHandler
  )
where

import Control.Natural (type (~>))
import Control.Monad.Trans.Free (FreeT, liftF)

import Control.Effect.Base
  ( Effect
  , EffFunctor (..)
  , FreeEff (..)
  , EffOps (..)
  , UnionOps (..)
  , Normalizable (..)
  )

import Control.Effect.Computation
  (BaseHandler, baseHandler)

data IoEff where

data IoOps eff = IoOps {
  liftIoOp :: forall a . IO a -> eff a
}

data IoModel a = LiftIO (IO a)

type IoConstraint eff = (?ioOps :: IoOps eff)

instance Functor IoModel where
  fmap f (LiftIO io) = LiftIO $ fmap f io

instance EffFunctor IoOps where
  effmap liftEff ops = IoOps {
    liftIoOp = liftEff . liftIoOp ops
  }

instance FreeEff IoEff where
  type Operation IoEff = IoOps
  type CoOperation IoEff = IoModel

  freeOps = freeIoOps

instance EffOps IoEff where
  type OpsConstraint IoEff eff = (IoConstraint eff)

  bindConstraint ops comp = let ?ioOps = ops in comp

  captureOps = ?ioOps

instance Normalizable IoEff where
  unionOps = UnionOps

liftIo :: forall a eff .
  (IoConstraint eff)
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

ioOps :: IoOps IO
ioOps = IoOps {
  liftIoOp = id
}

ioHandler :: BaseHandler IoEff IO
ioHandler = baseHandler IoOps {
  liftIoOp = id
}
