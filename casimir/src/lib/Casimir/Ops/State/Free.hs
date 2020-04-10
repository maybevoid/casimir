{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Ops.State.Free
  ( StateCoOp (..)
  )
where

import Casimir.Ops.State.Base

import Casimir.Free

data StateCoOp s r =
    GetOp (s -> r)
  | PutOp s (() -> r)

instance Functor (StateCoOp s) where
  fmap f (GetOp cont) = GetOp $ fmap f cont
  fmap f (PutOp s cont) = PutOp s $ fmap f cont

instance EffCoOp (StateEff s) where
  type CoOperation (StateEff s) = StateCoOp s

instance FreeOps (StateEff s) where
  mkFreeOps liftCoOp = StateOps {
    getOp = liftCoOp $ GetOp id,
    putOp = \x -> liftCoOp $ PutOp x id
  }
