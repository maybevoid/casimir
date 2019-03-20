
module Benchmark.State.HandleFree
where

import Control.Effect
import Benchmark.State.Base

handleFreeComp
  :: forall free eff
   . (FreeEff free, Effect eff)
  => Int
  -> eff ()
handleFreeComp s = do
  coState <- withCoOpHandler @free stateCoOpHandler stateBaseFunc
  runCoState s coState

handleFreerComp
  :: forall free eff
   . (FreerEff free, Effect eff)
  => Int
  -> eff ()
handleFreerComp s = do
  coState <- withFreerCoOpHandler @free stateFreerCoOpHandler stateBaseFunc
  runCoState s coState
