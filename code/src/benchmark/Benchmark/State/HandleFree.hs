
module Benchmark.State.HandleFree
where

import Control.Effect.Implicit

import qualified Control.Effect.Implicit.Free as Free
import qualified Control.Effect.Implicit.Free.Handler as Free

import qualified Control.Effect.Implicit.Freer as Freer

import Benchmark.State.Base

handleFreeComp
  :: forall free eff
   . (Free.FreeHandler free, Effect eff)
  => Int
  -> eff ()
handleFreeComp s
  = Free.withContextualCoOpHandler @free
    stateCoOpHandler
    (runCoState s)
    stateBaseFunc

handleFreerComp
  :: forall free eff
   . (Freer.FreeEff free, Effect eff)
  => Int
  -> eff ()
handleFreerComp s =
  Freer.withCoOpHandler @free
    stateFreerCoOpHandler
    stateBaseFunc
  >>= runCoState s
