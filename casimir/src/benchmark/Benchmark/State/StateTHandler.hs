
module Benchmark.State.StateTHandler
where

import Control.Monad.Trans.State.Strict (StateT)

import Casimir

import Benchmark.State.Base

stateHComp1
  :: forall m . (Monad m)
  => BaseComputation NoEff (Return ()) (StateT Int m)
stateHComp1 = bindOpsHandler
  stateTHandler stateBaseComp

stateTHandlerComp
  :: forall m . (Monad m)
  => StateT Int m ()
stateTHandlerComp = returnVal $ runComp stateHComp1 idLift NoOp
