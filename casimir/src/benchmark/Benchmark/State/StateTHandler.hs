
module Benchmark.State.StateTHandler
where

import Control.Monad.Trans.State.Strict (StateT)

import Casimir

import Benchmark.State.Base

stateHComp1
  :: forall eff . (Monad eff)
  => BaseComputation NoEff (Return ()) (StateT Int eff)
stateHComp1 = bindOpsHandler
  stateTHandler stateBaseComp

stateTHandlerComp
  :: forall eff . (Monad eff)
  => StateT Int eff ()
stateTHandlerComp = returnVal $ runComp stateHComp1 idLift NoOp
