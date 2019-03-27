
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

data LogCoOp l r = LogOp l (() -> r)
  deriving (Functor)

type LogConstraint l eff = (?logOps :: LogOps l eff)

instance EffFunctor (LogOps l) where
  effmap lift ops = LogOps $
    lift . logOp ops

instance FreeOps (LogEff l) where
  type Operation (LogEff l) = LogOps l
  type CoOperation (LogEff l) = LogCoOp l

  mkFreeOps liftCoOp = LogOps $
    \l -> liftCoOp $ LogOp l id

instance EffOps (LogEff l) where
  type OpsConstraint (LogEff l) eff = LogConstraint l eff

  withOps ops comp = let ?logOps = ops in comp
  captureOps = ?logOps

log
  :: forall l eff
   . (Effect eff, LogConstraint l eff)
  => l
  -> eff ()
log l = logOp captureOps l

stateLoggerHandler
  :: forall l eff
   . (Effect eff)
  => Handler (StateEff [l]) (LogEff l) eff eff
stateLoggerHandler = genericHandler $ LogOps $
  \l -> do
    logs <- get
    put $ l : logs

printLoggerHandler
  :: forall a eff
   . (Effect eff, Show a)
  => Handler IoEff (LogEff a) eff eff
printLoggerHandler = genericHandler $ LogOps $
  liftIo . print