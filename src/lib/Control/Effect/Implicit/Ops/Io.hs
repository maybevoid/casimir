
module Control.Effect.Implicit.Ops.Io
  ( IoEff
  , IoOps (..)
  , IoCoOp (..)
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

instance EffOps IoEff where
  type Operation IoEff = IoOps

instance EffCoOp IoEff where
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

instance ImplicitOps IoEff where
  type OpsConstraint IoEff eff =
    (?_Control_Effect_Implicit_Ops_Io_ioOps :: IoOps eff)

  withOps ops comp =
    let
      ?_Control_Effect_Implicit_Ops_Io_ioOps =
        ops in comp

  captureOps =
    ?_Control_Effect_Implicit_Ops_Io_ioOps

liftIo :: forall a eff .
  (EffConstraint IoEff eff)
  => IO a
  -> eff a
liftIo = liftIoOp captureOps

ioOps :: IoOps IO
ioOps = IoOps {
  liftIoOp = id
}

ioHandler :: BaseOpsHandler IoEff IO
ioHandler = baseOpsHandler IoOps {
  liftIoOp = id
}
