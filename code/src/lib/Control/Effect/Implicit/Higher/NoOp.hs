{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Higher.NoOp
where

import Data.Kind

import Control.Effect.Implicit.Base.NoOp (NoEff, NoOp (..))
import Control.Effect.Implicit.Freer.NoOp

import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.CoOp

class NoConstraint (eff1 :: Type -> Type) (eff2 :: Type -> Type)
instance NoConstraint eff1 eff2

instance EffOps NoEff where
  type Operation NoEff = HigherOps NoOp

instance EffCoOp NoEff where
  type CoOperation NoEff = HigherCoOp NoCoOp

instance HigherEffOps NoEff
instance HigherEffCoOp NoEff
