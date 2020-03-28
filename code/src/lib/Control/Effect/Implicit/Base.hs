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
  , BaseOps
  , Eff
  , EffConstraint
  , NoEff
  , NoOp (..)
  , LiftEff
  , type (∪)
  , NoConstraint
  , Union
  , UnionOps (..)
  , ContraLift (..)
  , (∪)
  , leftOps
  , rightOps
  , idLift
  , mkLiftEff
  , runLiftEff
  , applyEffmap
  , joinLift
  , module Control.Implicit.Param
)
where

import Control.Implicit.Param
import Control.Effect.Implicit.Base.Base
import Control.Effect.Implicit.Base.Effect
import Control.Effect.Implicit.Base.EffFunctor
import Control.Effect.Implicit.Base.Implicit
import Control.Effect.Implicit.Base.BaseOps
import Control.Effect.Implicit.Base.Union
import Control.Effect.Implicit.Base.NoOp
import Control.Effect.Implicit.Base.Lift
import Control.Effect.Implicit.Base.ContraLift
