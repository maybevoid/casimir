{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Ops.State.Higher
  (
  )
where

import Casimir.Ops.State.Base
import Casimir.Ops.State.Freer

import Casimir.Higher

instance Effect (State s) where
  type Operation (State s) =
    HigherOps (StateOps s)

instance EffCoOp (State s) where
  type CoOperation (State s) =
    HigherCoOp (StateCoOp s)

instance HigherEffect (State s)
instance HigherEffCoOp (State s)
