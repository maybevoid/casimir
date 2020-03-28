
module Benchmark.State.CurriedStateTPipeline
  ( curriedStateTComp
  )
where

import Control.Effect.Implicit
import Control.Effect.Implicit.Ops.State.Transform

import Benchmark.State.Base

curriedStateTComp :: forall eff . (Effect eff)
  => Int
  -> BaseComputation NoEff (Return ()) eff
curriedStateTComp i = runPipeline
  (stateTPipeline i)
  stateBaseComp
