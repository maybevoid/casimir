{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Ops.State.Higher
  (
  )
where

import Casimir.Ops.State.Base
import Casimir.Ops.State.Freer

import Casimir.Higher

instance Effects (State s) where
  type Operations (State s) =
    HigherOps (StateOps s)
