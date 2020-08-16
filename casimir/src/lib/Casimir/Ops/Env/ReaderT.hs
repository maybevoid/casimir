module Casimir.Ops.Env.ReaderT where

import Casimir.Ops.Env.Effect

import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as ReaderT

envOps :: forall e m . (Monad m) => EnvOps e (ReaderT e m)
envOps = EnvOps
  { askOp = ReaderT.ask
  }
