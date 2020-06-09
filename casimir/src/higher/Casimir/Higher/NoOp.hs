{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Higher.NoOp
where

import Data.Kind

import Casimir.Base (NoEff, NoOp (..))

import Casimir.Freer (NoCoOp)
import Casimir.Higher.Base
import Casimir.Higher.CoOp

class NoConstraint (m1 :: Type -> Type) (m2 :: Type -> Type)
instance NoConstraint m1 m2

instance Effects NoEff where
  type Operations' NoEff = HigherOps NoOp

instance EffCoOp NoEff where
  type CoOperation NoEff = HigherCoOp NoCoOp

instance HigherEffect NoEff
instance HigherEffCoOp NoEff
