
module Control.Effect.Implicit.Ops.Io
  ( IoEff
  , IoOps (..)
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
data IoEff

data IoOps eff = IoOps {
  liftIoOp :: forall a . IO a -> eff a
}

data IoCoOp a where
  LiftIoOp :: forall x a . IO x -> (x -> a) -> IoCoOp a

data IoCoOp' r where
  LiftIoOp' :: IO a -> IoCoOp' a

instance EffOps IoEff where
  type Operation IoEff = IoOps

instance Free.EffCoOp IoEff where
  type CoOperation IoEff = IoCoOp

instance Freer.EffCoOp IoEff where
  type CoOperation IoEff = IoCoOp'

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

instance Free.FreeOps IoEff where
  mkFreeOps liftCoOp = IoOps {
    liftIoOp = \io -> liftCoOp $ LiftIoOp io id
  }

instance Freer.FreeOps IoEff where
  mkFreeOps liftCoOp = IoOps {
    liftIoOp = \io -> liftCoOp $ LiftIoOp' io
  }

instance ImplicitOps IoEff where
  type OpsConstraint IoEff eff =
    TaggedOps IoTag IoEff eff

  withOps = withTaggedOps @IoTag
  captureOps = captureTaggedOps @IoTag

liftIo :: forall a . IO a -> Eff IoEff a
liftIo = liftIoOp captureOps

ioOps :: IoOps IO
ioOps = IoOps {
  liftIoOp = id
}

ioHandler :: BaseOpsHandler IoEff IO
ioHandler = baseOpsHandler IoOps {
  liftIoOp = id
}
