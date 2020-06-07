{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Base.NoOp
  ( NoEff
  , NoOp
  , pattern NoOp
  )
where

import qualified QuasiParam.Casimir as Param

import Casimir.Base.Lift
import Casimir.Base.Effect
import Casimir.Base.EffFunctor

data NoEff

type NoOp = Param.Nil
{-# COMPLETE NoOp #-}

pattern NoOp :: forall m . NoOp m
pattern NoOp = Param.Nil

instance Effects NoEff where
  type Operations NoEff = NoOp

instance EffFunctor Lift NoOp where
  effmap _ _ = NoOp
