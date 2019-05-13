
module Benchmark.State.CurriedStateTPipeline
  ( curriedStateTComp
  )
where

import Control.Effect.Implicit
import Control.Effect.Implicit.Transform.State

import Benchmark.State.Base

curriedStateTComp :: forall eff . (Effect eff)
  => Int
  -> Computation NoEff (Return ()) eff
curriedStateTComp i = runPipelineWithCast
  cast cast
  (stateTPipeline i)
  stateBaseComp

