
module Control.Effect.Implicit.Ops.Log
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation

import qualified Control.Effect.Implicit.Free as Free
import qualified Control.Effect.Implicit.Freer as Freer

import Control.Effect.Implicit.Ops.Io
import Control.Effect.Implicit.Ops.State

data LogOps l eff = LogOps {
  logOp :: l -> eff ()
}

data LogCoOp l r =
  LogOp l (() -> r)
  deriving (Functor)

data LogCoOp' l r where
  LogOp' :: l -> LogCoOp' l ()

instance Free.EffCoOp (LogOps l) where
  type CoOperation (LogOps l) = LogCoOp l

instance Freer.EffCoOp (LogOps l) where
  type CoOperation (LogOps l) = LogCoOp' l

instance EffFunctor (LogOps l) where
  effmap lift ops = LogOps $
    lift . logOp ops

instance Free.FreeOps (LogOps l) where
  mkFreeOps liftCoOp = LogOps $
    \l -> liftCoOp $ LogOp l id

instance Freer.FreeOps (LogOps l) where
  mkFreeOps liftCoOp = LogOps $
    \l -> liftCoOp $ LogOp' l

instance ImplicitOps (LogOps l) where
  type OpsConstraint (LogOps l) eff =
    (?_Control_Effect_Implicit_Ops_Log_logOps :: LogOps l eff)

  withOps ops comp =
    let
      ?_Control_Effect_Implicit_Ops_Log_logOps =
        ops in comp

  captureOps =
    ?_Control_Effect_Implicit_Ops_Log_logOps

log :: forall l . l -> Eff (LogOps l) ()
log l = logOp captureOps l

stateLoggerHandler
  :: forall l eff
   . (Effect eff)
  => OpsHandler (StateOps [l]) (LogOps l) eff
stateLoggerHandler = genericOpsHandler $ LogOps $
  \l -> do
    logs <- get
    put $ l : logs

printLoggerHandler
  :: forall a eff
   . (Effect eff, Show a)
  => OpsHandler IoOps (LogOps a) eff
printLoggerHandler = genericOpsHandler $ LogOps $
  liftIo . print
