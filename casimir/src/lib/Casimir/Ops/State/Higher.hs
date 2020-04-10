{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Ops.State.Higher
  (
  )
where

import Casimir.Ops.State.Base
import Casimir.Ops.State.Freer

import Casimir.Higher

instance EffOps (StateEff s) where
  type Operation (StateEff s) =
    HigherOps (StateOps s)

instance EffCoOp (StateEff s) where
  type CoOperation (StateEff s) =
    HigherCoOp (StateCoOp s)

instance HigherEffOps (StateEff s)
instance HigherEffCoOp (StateEff s)
