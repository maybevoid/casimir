module Casimir.Ops.State.MonadState where

import Casimir.Ops.State.Effect

import Control.Monad.State (MonadState)
import qualified Control.Monad.State as MonadState

stateOps :: forall s m . (MonadState s m) => StateOps s m
stateOps = StateOps
  { getOp = MonadState.get
  , putOp = MonadState.put
  }
