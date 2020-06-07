{-|
  Module      : Casimir.Base

  Base module that defines the basic datatypes for implicit-mects.
-}
module Casimir.Base
  ( Effects (..)
  , EffFunctor (..)
  , ImplicitOps
  , Eff
  , EffConstraint
  , NoEff
  , NoOp
  , Lift (..)
  , MaybeLift (..)
  , LiftMonoid (..)
  , LiftFunctor (..)
  , FreeLift (..)
  , HigherLift (..)
  , type (∪)
  , type (~>)
  , Union
  , UnionOps
  , ContraLift (..)
  , EntailOps
  , CastOps
  , type (⊇)
  , (∪)
  , pattern NoOp
  , pattern UnionOps
  , castOps
  , leftOps
  , rightOps
  , joinContraLift
  , identityContraLift
)
where

import Casimir.Base.Effect
import Casimir.Base.EffFunctor
import Casimir.Base.Implicit
import Casimir.Base.Union
import Casimir.Base.NoOp
import Casimir.Base.Lift
import Casimir.Base.ContraLift
import Casimir.Base.Cast
