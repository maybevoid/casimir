{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Ops.State.Freer
  ( StateCoOp (..)
  )
where

import Casimir.Ops.State.Base

import Casimir.Freer

data StateCoOp s a where
  GetOp :: StateCoOp s s
  PutOp :: s -> StateCoOp s ()

instance FreeOps (StateOps s) where
  type CoOperation (StateOps s) = StateCoOp s

  mkFreeOps liftCoOp = StateOps {
    getOp = liftCoOp $ GetOp,
    putOp = \x -> liftCoOp $ PutOp x
  }
