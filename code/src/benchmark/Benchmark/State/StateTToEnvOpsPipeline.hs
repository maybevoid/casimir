
module Benchmark.State.StateTToEnvOpsPipeline
  ( stateToReaderComp

  -- The simple action of exporting a specialized
  -- Identity base monad improves performance by ~25%
  , stateToReaderIdentityComp
  )
where

import Control.Monad.Identity
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)

import Control.Effect.Implicit
import Control.Effect.Implicit.Ops.Env
import Control.Effect.Implicit.Ops.State
import Control.Effect.Implicit.Transform.State

import Benchmark.State.Base

stateTToEnvOpsPipeline
  :: forall s eff1 comp .
  (Effect eff1, EffFunctor comp)
  => SimplePipeline LiftEff (EnvEff s) (StateEff s) comp eff1
stateTToEnvOpsPipeline = transformePipeline $ genericComputation handler
 where
  handler :: forall eff
   . (EffConstraint (EnvEff s) eff)
    => TransformerHandler (StateT s) (StateEff s) eff
  handler = TransformerHandler stateTOps stateTLiftEff $ mkLiftEff $
    \comp -> do
      i <- ask
      evalStateT comp i

stateTComp1 :: forall eff . (Effect eff)
  => BaseComputation (EnvEff Int) (Return ()) eff
stateTComp1 = runPipeline
  stateTToEnvOpsPipeline stateBaseComp

stateTComp2 :: forall eff . (Effect eff)
  => BaseComputation NoEff (Return ()) (ReaderT Int eff)
stateTComp2 = bindOpsHandler
  readerTHandler stateTComp1

stateToReaderComp :: forall eff . (Effect eff)
  => ReaderT Int eff ()
stateToReaderComp = returnVal $ runComp stateTComp2 idLift NoOp

stateToReaderIdentityComp :: ReaderT Int Identity ()
stateToReaderIdentityComp = stateToReaderComp
