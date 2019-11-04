{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Higher.Ops.State
where

import Control.Effect.Implicit.Higher

import Control.Effect.Implicit.Ops.State
  (StateEff, StateOps (..), FreerStateCoOp (..))

instance EffOps (StateEff s) where
  type Operation (StateEff s) = UpperOps (StateOps s)

instance EffCoOp (StateEff s) where
  type CoOperation (StateEff s) = UpperCoOp (FreerStateCoOp s)

instance HigherOps (StateEff s)
instance HigherCoOp (StateEff s)