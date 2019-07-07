{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Reflect.State
where

import Data.Constraint
import Control.Monad.State.Class (MonadState  (..))

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Ops.State

import Control.Effect.Implicit.Reflect.Reflect

instance
  ( Effect eff
  , ReifiesOps (StateEff s) eff
  )
  => MonadState s (ReflectM (StateEff s) eff)
  where
    get = ReflectM $ getOp $
      reflectOps @(StateEff s)

    put s = ReflectM $ putOp
      (reflectOps @(StateEff s))
      s

instance
  ReflectOps (StateEff s)
  where
    type OpsClass (StateEff s) eff
      = MonadState s eff

    opsDict ops = withReifiedOps ops $
      castClass Dict
