
module Control.Effect.Implicit.Base
  ( Effect
  , EffFunctor (..)
  , FreeOps (..)
  , EffOps (..)
  , EffConstraint
  , NoEff
  , Union
  , type (∪)
  , TaggedEff
  , LiftEff (..)
  , NoOp (..)
  , NoCoOp (..)
  , NoConstraint
  , UnionOps (..)
  , (∪)
  , UnionCoOp (..)
  , TaggedOps (..)
  , TaggedCoOp (..)
  , idLift
  , joinLift
  , mkLiftEff
  , leftOps
  , rightOps
  , untagOps
  , untagCoOp
  , withTag
  )
where

import Control.Effect.Implicit.Base.Effect
import Control.Effect.Implicit.Base.EffFunctor
import Control.Effect.Implicit.Base.LiftEff
import Control.Effect.Implicit.Base.FreeOps
import Control.Effect.Implicit.Base.EffOps
import Control.Effect.Implicit.Base.Tag
import Control.Effect.Implicit.Base.Union
import Control.Effect.Implicit.Base.NoEff