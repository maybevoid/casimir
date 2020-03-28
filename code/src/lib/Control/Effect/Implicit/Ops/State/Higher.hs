{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Ops.State.Higher
  (
  )
where

import Control.Effect.Implicit.Ops.State.Base
import Control.Effect.Implicit.Ops.State.Freer

import Control.Effect.Implicit.Higher

instance EffOps (StateEff s) where
  type Operation (StateEff s) =
    HigherOps (StateOps s)

instance EffCoOp (StateEff s) where
  type CoOperation (StateEff s) =
    HigherCoOp (StateCoOp s)

instance HigherEffOps (StateEff s)
instance HigherEffCoOp (StateEff s)
