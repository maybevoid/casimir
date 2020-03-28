{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Ops.State.Freer
  ( StateCoOp (..)
  )
where

import Control.Effect.Implicit.Ops.State.Base

import Control.Effect.Implicit.Freer

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
