{-|
  Module      : Control.Effect.Implicit.Base

  Base module that defines the basic datatypes for implicit-effects.
-}
module Control.Effect.Implicit.Base
  ( Effect
  , EffFunctor (..)
  , ImplicitOps (..)
  , BaseOps
  , Eff
  , EffConstraint
  , NoOp (..)
  , LiftEff
  , type (∪)
  , NoConstraint
  , Union (..)
  , (∪)
  , leftOps
  , rightOps
  , idLift
  , mkLiftEff
  , liftEff
  , applyEffmap
  , joinLift
  , module Control.Implicit.Param
)
where

import Control.Implicit.Param
import Control.Effect.Implicit.Base.Effect
import Control.Effect.Implicit.Base.EffFunctor
import Control.Effect.Implicit.Base.Implicit
import Control.Effect.Implicit.Base.BaseOps
import Control.Effect.Implicit.Base.Union
import Control.Effect.Implicit.Base.NoOp
import Control.Effect.Implicit.Base.Lift