{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Higher.NoOp
where

import Data.Kind

import Casimir.Base.NoOp (NoEff, NoOp (..))
import Casimir.Freer.NoOp

import Casimir.Higher.Base
import Casimir.Higher.CoOp

class NoConstraint (eff1 :: Type -> Type) (eff2 :: Type -> Type)
instance NoConstraint eff1 eff2

instance EffOps NoEff where
  type Operation NoEff = HigherOps NoOp

instance EffCoOp NoEff where
  type CoOperation NoEff = HigherCoOp NoCoOp

instance HigherEffOps NoEff
instance HigherEffCoOp NoEff
