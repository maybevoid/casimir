
module Benchmark.State.CurriedStateTPipeline
  ( curriedStateTComp
  )
where

import Casimir
import Casimir.Ops.State.Transform

import Benchmark.State.Base

curriedStateTComp :: forall eff . (Monad eff)
  => Int
  -> BaseComputation NoEff (Return ()) eff
curriedStateTComp i = runPipeline
  (stateTPipeline i)
  stateBaseComp
