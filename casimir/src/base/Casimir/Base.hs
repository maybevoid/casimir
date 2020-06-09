{-|
  Module      : Casimir.Base

  Base module that defines the basic datatypes for implicit-mects.
-}
module Casimir.Base
  ( Effect (..)
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
  , Effects (..)

  , ContraLift (..)

  , CastOps
  , EntailOps
  , CastDict
  , EntailDict

  , type (∪)
  , type (~>)
  , type (⊇)

  , (∪)
  , pattern Cons
  , pattern NoOp
  , pattern Union

  , withOp
  , withOps
  , captureOp
  , captureOps
  , leftOps
  , rightOps
  , joinContraLift
  , identityContraLift

  , castOps
  , castDict
  , entailDict
  , extendCast
  , composeCast
  , castOpsWithDict

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
