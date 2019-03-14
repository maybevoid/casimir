
module Control.Effect.Ops.Io
  ( IoEff
  , IoOps (..)
  , IoCoOps (..)
  , IoConstraint
  , liftIo
  , ioOps
  , ioHandler
  )
where

import Control.Effect.Base
  ( EffFunctor (..)
  , FreeOps (..)
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

data IoCoOps a = LiftIO (IO a)

type IoConstraint eff = (?ioOps :: IoOps eff)

instance Functor IoCoOps where
  fmap f (LiftIO io) = LiftIO $ fmap f io

instance EffFunctor IoOps where
  effmap liftEff ops = IoOps {
    liftIoOp = liftEff . liftIoOp ops
  }

instance FreeOps IoEff where
  type Operation IoEff = IoOps
  type CoOperation IoEff = IoCoOps

  mkFreeOps liftCoOps = IoOps {
    liftIoOp = \io -> liftCoOps $ LiftIO io
  }

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

ioOps :: IoOps IO
ioOps = IoOps {
  liftIoOp = id
}

ioHandler :: BaseHandler IoEff IO
ioHandler = baseHandler IoOps {
  liftIoOp = id
}
