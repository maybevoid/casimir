
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

instance EffCoOp IoEff where
  type CoOperation IoEff = IoCoOp

instance FreerEffCoOp IoEff where
  type FreerCoOp IoEff = IoCoOp'

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

instance FreeOps IoEff where
  mkFreeOps liftCoOp = IoOps {
    liftIoOp = \io -> liftCoOp $ LiftIoOp io id
  }

instance FreerOps IoEff where
  mkFreerOps liftCoOp = IoOps {
    liftIoOp = \io -> liftCoOp $ LiftIoOp' io
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
