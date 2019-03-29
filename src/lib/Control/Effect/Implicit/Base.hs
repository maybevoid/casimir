{-|
  Module      : Control.Effect.Implicit.Base

  Base module that defines the basic datatypes for implicit-effects.
-}
module Control.Effect.Implicit.Base
  ( Effect
  , EffFunctor (..)
  , EffSpec (..)
  , ImplicitOps (..)
  , FreeOps (..)
  , EffOps
  , EffConstraint
  , NoEff
  , Union
  , LiftEff
  , type (∪)
  , NoOp (..)
  , NoCoOp (..)
  , NoConstraint
  , UnionOps (..)
  , (∪)
  , UnionCoOp (..)
  , leftOps
  , rightOps
  , idLift
  , mkLiftEff
  , liftEff
  , applyEffmap
  , joinLift
  )
where

import Control.Effect.Implicit.Base.Effect
import Control.Effect.Implicit.Base.EffFunctor
import Control.Effect.Implicit.Base.EffSpec
import Control.Effect.Implicit.Base.Implicit
import Control.Effect.Implicit.Base.FreeOps
import Control.Effect.Implicit.Base.EffOps
import Control.Effect.Implicit.Base.Union
import Control.Effect.Implicit.Base.NoEff
import Control.Effect.Implicit.Base.LiftEff