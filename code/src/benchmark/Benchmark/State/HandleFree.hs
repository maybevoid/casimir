
module Benchmark.State.HandleFree
where

import Control.Effect.Implicit
import Control.Effect.Implicit.Free
import Control.Effect.Implicit.Freer

import Benchmark.State.Base

handleFreeComp
  :: forall free eff
   . (FreeEff free, Effect eff)
  => Int
  -> eff ()
handleFreeComp s
  = withContextualCoOpHandler @free
    stateCoOpHandler
    (runCoState s)
    stateBaseFunc

handleFreerComp
  :: forall free eff
   . (FreerEff free, Effect eff)
  => Int
  -> eff ()
handleFreerComp s =
  withFreerCoOpHandler @free
    stateFreerCoOpHandler
    stateBaseFunc
  >>= runCoState s
