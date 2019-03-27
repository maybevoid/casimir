
module Control.Effect.Implicit.Ops.Io
  ( IoEff
  , IoOps (..)
  , IoCoOp (..)
  , IoConstraint
  , liftIo
  , ioOps
  , ioHandler
  )
where

import Control.Effect.Implicit.Base
  ( EffFunctor (..)
  , FreeOps (..)
  , EffOps (..)
  )

import Control.Effect.Implicit.Computation
  (BaseHandler, baseHandler)

data IoEff where

data IoOps eff = IoOps {
  liftIoOp :: forall a . IO a -> eff a
}

data IoCoOp a where
  IoCoOp :: forall x a . IO x -> (x -> a) -> IoCoOp a

type IoConstraint eff = (?ioOps :: IoOps eff)

instance Functor IoCoOp where
  fmap
    :: forall a b
     . (a -> b)
    -> IoCoOp a
    -> IoCoOp b
  fmap f (IoCoOp io cont) = IoCoOp io (f . cont)

instance EffFunctor IoOps where
  effmap liftEff ops = IoOps {
    liftIoOp = liftEff . liftIoOp ops
  }

instance FreeOps IoEff where
  type Operation IoEff = IoOps
  type CoOperation IoEff = IoCoOp

  mkFreeOps liftCoOp = IoOps {
    liftIoOp = \io -> liftCoOp $ IoCoOp io id
  }

instance EffOps IoEff where
  type OpsConstraint IoEff eff = (IoConstraint eff)

  withOps ops comp = let ?ioOps = ops in comp

  captureOps = ?ioOps

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
