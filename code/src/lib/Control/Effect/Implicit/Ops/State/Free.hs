{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Ops.State.Free
  ( StateCoOp (..)
  )
where

import Control.Effect.Implicit.Ops.State.Base

import Control.Effect.Implicit.Free

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
