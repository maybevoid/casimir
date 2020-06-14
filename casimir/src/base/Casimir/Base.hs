{-|
  Module      : Casimir.Base

  Base module that defines the basic datatypes for implicit-mects.
-}
module Casimir.Base
  ( Effect
  , Effects
  , Union (..)
  , Cons (..)
  , Nil (..)
  , Singleton (..)
  , Multi
  , OpsConstraint
  , EffConstraint
  , Eff
  , withOps
  , captureOps
  , withOp
  , captureOp
  , type (∪)
  , pattern (:+)

  , EffFunctor (..)

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
import Casimir.Base.Lift
import Casimir.Base.ContraLift
import Casimir.Base.Cast

import QuasiParam.Tag (Tag)
import QuasiParam.Name (Name)
import QuasiParam.Label (Label, HasLabel (..))
