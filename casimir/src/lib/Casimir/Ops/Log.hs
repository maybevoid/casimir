
module Casimir.Ops.Log
where

import Casimir.Base
import Casimir.Computation

import qualified Casimir.Free as Free
import qualified Casimir.Freer as Freer

import Casimir.Ops.Io
import Casimir.Ops.State

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

instance Free.EffCoOp (LogEff l) where
  type CoOperation (LogEff l) = LogCoOp l

instance Freer.EffCoOp (LogEff l) where
  type CoOperation (LogEff l) = LogCoOp' l

instance EffFunctor Lift (LogOps l) where
  effmap (Lift lift) ops = LogOps $
    lift . logOp ops

instance Free.FreeOps (LogEff l) where
  mkFreeOps liftCoOp = LogOps $
    \l -> liftCoOp $ LogOp l id

instance Freer.FreeOps (LogEff l) where
  mkFreeOps liftCoOp = LogOps $
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

log :: forall l . l -> Eff (LogEff l) ()
log l = logOp captureOps l

stateLoggerHandler
  :: forall l eff
   . (Effect eff)
  => BaseOpsHandler (StateEff [l]) (LogEff l) eff
stateLoggerHandler = genericOpsHandler $ LogOps $
  \l -> do
    logs <- get
    put $ l : logs

printLoggerHandler
  :: forall a eff
   . (Effect eff, Show a)
  => BaseOpsHandler IoEff (LogEff a) eff
printLoggerHandler = genericOpsHandler $ LogOps $
  liftIo . print
