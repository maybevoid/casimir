{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.MonadOps.Ops
where

import Control.Monad.Trans.State.Strict (StateT)

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.MonadOps.MonadOps

import Control.Effect.Implicit.Ops.Io
import Control.Effect.Implicit.Ops.State

import Control.Effect.Implicit.Transform.State

data UseIo
data UseState s t
data UseHigherState s t

instance MonadOps UseIo where
  type HasOps UseIo = IoEff

  type OpsMonad UseIo = IO

  monadOps = ioOps

instance
  ( BaseMonadOps t )
  => MonadOps (UseState s t) where
    type HasOps (UseState s t) = StateEff s ∪ HasOps t

    type OpsMonad (UseState s t) = StateT s (OpsMonad t)

    monadOps = stateTOps ∪ effmap liftStateT (monadOps @t)

instance
  ( HigherMonadOps t )
  => MonadOps (UseHigherState s t) where
    type HasOps (UseHigherState s t) = StateEff s ∪ HasOps t

    type OpsMonad (UseHigherState s t) = StateT s (OpsMonad t)

    monadOps = stateTOps
      ∪ invEffmap liftStateT stateTContraLift (monadOps @t)
