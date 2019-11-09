{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Higher.NoOp
where

import Data.Kind

import Control.Effect.Implicit.Base.NoOp (NoEff, NoOp (..))
import Control.Effect.Implicit.Freer.NoOp

import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.CoOp
import Control.Effect.Implicit.Higher.Implicit
import Control.Effect.Implicit.Higher.UpperOps

class NoConstraint (eff1 :: Type -> Type) (eff2 :: Type -> Type)
instance NoConstraint eff1 eff2

instance EffOps NoEff where
  type Operation NoEff = UpperOps NoOp

instance EffCoOp NoEff where
  type CoOperation NoEff = UpperCoOp NoCoOp

instance HigherOps NoEff
instance HigherCoOp NoEff

instance ImplicitOps NoEff where
  type OpsConstraint NoEff eff1 eff2 =
    NoConstraint eff1 eff2

  withHigherOps _ = id

  captureHigherOps = UpperOps NoOp NoOp
