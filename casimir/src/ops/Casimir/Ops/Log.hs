
module Casimir.Ops.Log
where

import Casimir.Base
import Casimir.Freer

data LogTag
data LogEff l

data LogOps l m = LogOps {
  logOp :: l -> m ()
}

data LogCoOp l r where
  LogOp :: l -> LogCoOp l ()

instance EffFunctor Lift (LogOps l) where
  effmap (Lift lift) ops = LogOps $
    lift . logOp ops

instance FreeOps (LogOps l) where
  type CoOperation (LogOps l) = LogCoOp l

  mkFreeOps liftCoOp = LogOps $
    \l -> liftCoOp $ LogOp l

instance HasLabel (LogOps l) where
  type GetLabel (LogOps l) = Tag LogTag

log :: forall l . l -> Eff '[LogOps l] ()
log l = logOp captureOp l
