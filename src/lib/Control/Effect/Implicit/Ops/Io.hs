
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
import Control.Effect.Implicit.Computation

data IoEff

data IoOps eff = IoOps {
  liftIoOp :: forall a . IO a -> eff a
}

data IoCoOp a where
  IoCoOp :: forall x a . IO x -> (x -> a) -> IoCoOp a

instance EffSpec IoEff where
  type Operation IoEff = IoOps
  type CoOperation IoEff = IoCoOp

instance Functor IoCoOp where
  fmap
    :: forall a b
     . (a -> b)
    -> IoCoOp a
    -> IoCoOp b
  fmap f (IoCoOp io cont) = IoCoOp io (f . cont)

instance EffFunctor IoOps where
  effmap lifter ops = IoOps {
    liftIoOp = lifter . liftIoOp ops
  }

instance FreeOps IoEff where
  mkFreeOps liftCoOp = IoOps {
    liftIoOp = \io -> liftCoOp $ IoCoOp io id
  }

type IoConstraint eff = (?ioOps :: IoOps eff)

instance ImplicitOps IoEff where
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
