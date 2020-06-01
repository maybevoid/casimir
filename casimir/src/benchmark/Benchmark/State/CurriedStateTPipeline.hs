
module Benchmark.State.CurriedStateTPipeline
  ( curriedStateTComp
  )
where

import Casimir
import Casimir.Ops.State.Transform

import Benchmark.State.Base

curriedStateTComp :: forall m . (Monad m)
  => Int
  -> BaseComputation NoEff (Return ()) m
curriedStateTComp i = runPipeline
  (stateTPipeline i)
  stateBaseComp
