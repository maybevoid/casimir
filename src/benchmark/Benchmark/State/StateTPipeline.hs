
module Benchmark.State.StateTPipeline
  ( stateToReaderComp

  -- The simple action of exporting a specialized
  -- Identity base monad improves performance by ~25%
  , stateToReaderIdentityComp
  )
where

import Control.Monad.Identity
import Control.Monad.Trans.Reader (ReaderT)

import Control.Effect
import Benchmark.State.Base

stateTComp1 :: forall eff . (Effect eff)
  => Computation (EnvEff Int) (Return ()) eff
stateTComp1 = runPipelineWithCast
  stateTPipeline stateBaseComp
  cast cast

stateTComp2 :: forall eff . (Effect eff)
  => Computation NoEff (Return ()) (ReaderT Int eff)
stateTComp2 = bindHandlerWithCast
  readerTHandler stateTComp1
  cast cast

stateToReaderComp :: forall eff . (Effect eff)
  => ReaderT Int eff ()
stateToReaderComp = returnVal $ runComp stateTComp2 idLift NoOp

stateToReaderIdentityComp :: ReaderT Int Identity ()
stateToReaderIdentityComp = stateToReaderComp
