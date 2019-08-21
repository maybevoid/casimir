
module Benchmark.State.StateTHandler
where

import Control.Monad.Trans.State.Strict (StateT)

import Control.Effect.Implicit
import Control.Effect.Implicit.Transform.State

import Benchmark.State.Base

stateHComp1
  :: forall eff . (Effect eff)
  => Computation NoOp (Return ()) (StateT Int eff)
stateHComp1 = bindOpsHandler
  stateTHandler stateBaseComp

stateTHandlerComp
  :: forall eff . (Effect eff)
  => StateT Int eff ()
stateTHandlerComp = returnVal $ runComp stateHComp1 idLift NoOp
