{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Ops.Io.Freer
where

import Control.Effect.Implicit.Ops.Io.Base

import Control.Effect.Implicit.Freer

data IoCoOp r where
  LiftIoOp :: IO a -> IoCoOp a

instance EffCoOp IoEff where
  type CoOperation IoEff = IoCoOp

instance FreeOps IoEff where
  mkFreeOps liftCoOp = IoOps {
    liftIoOp = \io -> liftCoOp $ LiftIoOp io
  }
