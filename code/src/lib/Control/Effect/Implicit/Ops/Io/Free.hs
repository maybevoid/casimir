{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Ops.Io.Free
where

import Control.Effect.Implicit.Ops.Io.Base

import Control.Effect.Implicit.Free

data IoCoOp a where
  LiftIoOp :: forall x a . IO x -> (x -> a) -> IoCoOp a

instance EffCoOp IoEff where
  type CoOperation IoEff = IoCoOp

instance Functor IoCoOp where
  fmap
    :: forall a b
     . (a -> b)
    -> IoCoOp a
    -> IoCoOp b
  fmap f (LiftIoOp io cont) = LiftIoOp io (f . cont)

instance FreeOps IoEff where
  mkFreeOps liftCoOp = IoOps {
    liftIoOp = \io -> liftCoOp $ LiftIoOp io id
  }
