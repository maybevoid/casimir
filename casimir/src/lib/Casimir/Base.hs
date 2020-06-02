{-|
  Module      : Casimir.Base

  Base module that defines the basic datatypes for implicit-mects.
-}
module Casimir.Base
  ( EffOps (..)
  , EffFunctor (..)
  , ImplicitOps (..)
  , Eff
  , OpsConstraint
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
  , Union
  , UnionOps (..)
  , ContraLift (..)
  , LabeledEff
  , LabeledOps (..)
  , NamedEff
  , TaggedEff
  , NamedOps
  , TaggedOps
  , EntailOps (..)
  , AllCoercible (..)
  , type (⊇)
  , (∪)
  , withOps
  , captureOps
  , leftOps
  , rightOps
  , joinContraLift
  , identityContraLift
  , pattern NoOp
  , pattern UnionOps
)
where

import Casimir.Base.EffOps
import Casimir.Base.Label
import Casimir.Base.EffFunctor
import Casimir.Base.Implicit
import Casimir.Base.Union
import Casimir.Base.NoOp
import Casimir.Base.Lift
import Casimir.Base.ContraLift
