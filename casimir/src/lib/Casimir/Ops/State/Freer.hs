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

instance EffCoOp (StateEff s) where
  type CoOperation (StateEff s) = StateCoOp s

instance FreeOps (StateEff s) where
  mkFreeOps liftCoOp = StateOps {
    getOp = liftCoOp $ GetOp,
    putOp = \x -> liftCoOp $ PutOp x
  }
