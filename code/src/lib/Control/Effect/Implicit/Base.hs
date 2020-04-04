{-|
  Module      : Control.Effect.Implicit.Base

  Base module that defines the basic datatypes for implicit-effects.
-}
module Control.Effect.Implicit.Base
  ( Effect
  , EffOps (..)
  , EffFunctor (..)
  , HigherEffFunctor (..)
  , ImplicitOps (..)
  , Eff
  , EffConstraint
  , NoEff
  , NoOp (..)
  , Lift (..)
  , LiftOps (..)
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
  , module Control.Implicit.Param
)
where

import Control.Implicit.Param
import Control.Effect.Implicit.Base.EffOps
import Control.Effect.Implicit.Base.Effect
import Control.Effect.Implicit.Base.EffFunctor
import Control.Effect.Implicit.Base.Implicit
import Control.Effect.Implicit.Base.Union
import Control.Effect.Implicit.Base.NoOp
import Control.Effect.Implicit.Base.Lift
import Control.Effect.Implicit.Base.ContraLift
