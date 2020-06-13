{-|
  Module      : Casimir.Base

  Base module that defines the basic datatypes for implicit-mects.
-}
module Casimir.Base
  ( Effect (..)
  , Effects (..)
  , Union
  , Cons
  , NoEff
  , NoOp
  , ConsOps
  , UnionOps
  , type (∪)
  , (∪)
  , pattern (:∪)
  , pattern (:+)
  , pattern NoOp
  , pattern Cons
  , pattern Union

  , EffFunctor (..)

  , ImplicitOps
  , Eff
  , OpsConstraint
  , EffConstraint

  , Lift (..)
  , MaybeLift (..)
  , LiftMonoid (..)
  , LiftFunctor (..)
  , FreeLift (..)
  , HigherLift (..)

  , ContraLift (..)

  , CastOps
  , EntailOps
  , CastDict
  , EntailDict

  , type (~>)
  , type (⊇)

  , withOp
  , withOps
  , captureOp
  , captureOps
  , joinContraLift
  , identityContraLift

  , castOps
  , castDict
  , entailDict
  , extendCast
  , composeCast
  , castOpsWithDict

  , HasLabel (..)
  , Tag
  , Name
  , Label
)
where

import Casimir.Base.Effect
import Casimir.Base.EffFunctor
import Casimir.Base.Implicit
import Casimir.Base.Lift
import Casimir.Base.ContraLift
import Casimir.Base.Cast

import QuasiParam.Tag (Tag)
import QuasiParam.Name (Name)
import QuasiParam.Label (Label, HasLabel (..))
