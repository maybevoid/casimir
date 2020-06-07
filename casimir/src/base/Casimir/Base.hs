{-|
  Module      : Casimir.Base

  Base module that defines the basic datatypes for implicit-mects.
-}
module Casimir.Base
  ( Effect (..)
  , Effects (..)
  , EffFunctor (..)

  , ImplicitOps
  , Eff
  , OpsConstraint
  , EffConstraint

  , NoEff
  , NoOp

  , Lift (..)
  , MaybeLift (..)
  , LiftMonoid (..)
  , LiftFunctor (..)
  , FreeLift (..)
  , HigherLift (..)

  , Union
  , UnionOps
  , Cons
  , EffectList (..)
  , ContraLift (..)
  , CastOps
  , EntailOps

  , type (:+)
  , type (∪)
  , type (~>)
  , type (⊇)

  , (∪)
  , pattern Cons
  , pattern NoOp
  , pattern Union
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
import Casimir.Base.List
import Casimir.Base.Cast
