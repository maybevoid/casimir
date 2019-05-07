
module Control.Effect.Implicit.Ops.Log
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation

import Control.Effect.Implicit.Ops.Io
import Control.Effect.Implicit.Ops.State

data LogEff l

data LogOps l eff = LogOps {
  logOp :: l -> eff ()
}

data LogCoOp l r =
  LogOp l (() -> r)
  deriving (Functor)

data LogCoOp' l r where
  LogOp' :: l -> LogCoOp' l ()

instance EffOps (LogEff l) where
  type Operation (LogEff l) = LogOps l

instance EffCoOp (LogEff l) where
  type CoOperation (LogEff l) = LogCoOp l

instance FreerEffCoOp (LogEff l) where
  type FreerCoOp (LogEff l) = LogCoOp' l

instance EffFunctor (LogOps l) where
  effmap lift ops = LogOps $
    lift . logOp ops

instance FreeOps (LogEff l) where
  mkFreeOps liftCoOp = LogOps $
    \l -> liftCoOp $ LogOp l id

instance FreerOps (LogEff l) where
  mkFreerOps liftCoOp = LogOps $
    \l -> liftCoOp $ LogOp' l

instance ImplicitOps (LogEff l) where
  type OpsConstraint (LogEff l) eff =
    (?_Control_Effect_Implicit_Ops_Log_logOps :: LogOps l eff)

  withOps ops comp =
    let
      ?_Control_Effect_Implicit_Ops_Log_logOps =
        ops in comp

  captureOps =
    ?_Control_Effect_Implicit_Ops_Log_logOps

log
  :: forall l eff
   . (EffConstraint (LogEff l) eff)
  => l
  -> eff ()
log l = logOp captureOps l

stateLoggerHandler
  :: forall l eff
   . (Effect eff)
  => OpsHandler (StateEff [l]) (LogEff l) eff
stateLoggerHandler = genericOpsHandler $ LogOps $
  \l -> do
    logs <- get
    put $ l : logs

printLoggerHandler
  :: forall a eff
   . (Effect eff, Show a)
  => OpsHandler IoEff (LogEff a) eff
printLoggerHandler = genericOpsHandler $ LogOps $
  liftIo . print