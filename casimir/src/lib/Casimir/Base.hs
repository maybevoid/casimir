{-|
  Module      : Casimir.Base

  Base module that defines the basic datatypes for implicit-effects.
-}
module Casimir.Base
  ( Effect
  , EffOps (..)
  , EffFunctor (..)
  , ImplicitOps (..)
  , Eff
  , EffConstraint
  , NoEff
  , NoOp (..)
  , Lift (..)
  , MaybeLift (..)
  , LiftMonoid (..)
  , LiftFunctor (..)
  , FreeLift (..)
  , HigherLift (..)
  , type (∪)
  , type (~>)
  , NoConstraint
  , Union
  , UnionOps (..)
  , ContraLift (..)
  , (∪)
  , leftOps
  , rightOps
  , joinContraLift
  , identityContraLift
)
where

import Casimir.Base.EffOps
import Casimir.Base.Effect
import Casimir.Base.EffFunctor
import Casimir.Base.Implicit
import Casimir.Base.Union
import Casimir.Base.NoOp
import Casimir.Base.Lift
import Casimir.Base.ContraLift
