
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

data IoCoOps a where
  IoCoOps :: forall x a . IO x -> (x -> a) -> IoCoOps a

type IoConstraint eff = (?ioOps :: IoOps eff)

instance Functor IoCoOps where
  fmap
    :: forall a b
     . (a -> b)
    -> IoCoOps a
    -> IoCoOps b
  fmap f (IoCoOps io cont) = IoCoOps io (f . cont)

instance EffFunctor IoOps where
  effmap liftEff ops = IoOps {
    liftIoOp = liftEff . liftIoOp ops
  }

instance FreeOps IoEff where
  type Operation IoEff = IoOps
  type CoOperation IoEff = IoCoOps

  mkFreeOps liftCoOps = IoOps {
    liftIoOp = \io -> liftCoOps $ IoCoOps io id
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
