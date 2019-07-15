{-|
  Module      : Control.Effect.Implicit.Base

  Base module that defines the basic datatypes for implicit-effects.
-}
module Control.Effect.Implicit.Base
  ( Effect
  , EffFunctor (..)
  , EffOps (..)
  , ImplicitOps (..)
  , BaseOps
  , Eff
  , EffConstraint
  , NoEff
  , Union
  , LiftEff
  , type (∪)
  , NoOp (..)
  , NoConstraint
  , UnionOps (..)
  , (∪)
  , leftOps
  , rightOps
  , idLift
  , mkLiftEff
  , liftEff
  , applyEffmap
  , joinLift
  , module Control.Effect.Implicit.Base.Label
)
where

import Control.Effect.Implicit.Base.Effect
import Control.Effect.Implicit.Base.EffFunctor
import Control.Effect.Implicit.Base.EffOps
import Control.Effect.Implicit.Base.Implicit
import Control.Effect.Implicit.Base.BaseOps
import Control.Effect.Implicit.Base.Union
import Control.Effect.Implicit.Base.NoEff
import Control.Effect.Implicit.Base.Lift
import Control.Effect.Implicit.Base.Label
