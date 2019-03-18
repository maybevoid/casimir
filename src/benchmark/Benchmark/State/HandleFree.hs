
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
  let comp = withOps (freeOps @free @(StateEff Int)) stateBaseFunc
  coState <- handleFree @free stateCoOpHandler comp
  runCoState s coState