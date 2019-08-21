
module Control.Effect.Implicit.Ops.Io
  ( IoOps (..)
  , IoCoOp (..)
  , IoCoOp' (..)
  , liftIo
  , ioOps
  , ioHandler
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation

import qualified Control.Effect.Implicit.Free as Free
import qualified Control.Effect.Implicit.Freer as Freer

data IoTag

data IoOps eff = IoOps {
  liftIoOp :: forall a . IO a -> eff a
}

data IoCoOp a where
  LiftIoOp :: forall x a . IO x -> (x -> a) -> IoCoOp a

data IoCoOp' r where
  LiftIoOp' :: IO a -> IoCoOp' a

instance Free.EffCoOp IoOps where
  type CoOperation IoOps = IoCoOp

instance Freer.EffCoOp IoOps where
  type CoOperation IoOps = IoCoOp'

instance Functor IoCoOp where
  fmap
    :: forall a b
     . (a -> b)
    -> IoCoOp a
    -> IoCoOp b
  fmap f (LiftIoOp io cont) = LiftIoOp io (f . cont)

instance EffFunctor IoOps where
  effmap lifter ops = IoOps {
    liftIoOp = lifter . liftIoOp ops
  }

instance Free.FreeOps IoOps where
  mkFreeOps liftCoOp = IoOps {
    liftIoOp = \io -> liftCoOp $ LiftIoOp io id
  }

instance Freer.FreeOps IoOps where
  mkFreeOps liftCoOp = IoOps {
    liftIoOp = \io -> liftCoOp $ LiftIoOp' io
  }

instance ImplicitOps IoOps where
  type OpsConstraint IoOps eff =
    TaggedOpsParam IoTag IoOps eff

  withOps = withTag @IoTag
  captureOps = captureTag @IoTag

liftIo :: forall a . IO a -> Eff IoOps a
liftIo = liftIoOp captureOps

ioOps :: IoOps IO
ioOps = IoOps {
  liftIoOp = id
}

ioHandler :: BaseOpsHandler IoOps IO
ioHandler = baseOpsHandler IoOps {
  liftIoOp = id
}
