module Casimir.Ops.Env.MonadReader where

import Casimir.Ops.Env.Effect

import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as MonadReader

envOps :: forall e m . (MonadReader e m) => EnvOps e m
envOps = EnvOps
  { askOp = MonadReader.ask
  }
