{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Ops.Io.Freer
where

import Casimir.Ops.Io.Base

import Casimir.Freer

data IoCoOp r where
  LiftIoOp :: IO a -> IoCoOp a

instance FreeOps IoOps where
  type CoOperation IoOps = IoCoOp

  mkFreeOps liftCoOp = IoOps {
    liftIoOp = \io -> liftCoOp $ LiftIoOp io
  }
