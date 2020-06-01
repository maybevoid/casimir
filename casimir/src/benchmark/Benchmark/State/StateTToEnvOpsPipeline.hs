
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

import Casimir
import Casimir.Ops.Env
import Casimir.Ops.State
import Casimir.Ops.State.Lift
import Casimir.Ops.State.Transform

import Benchmark.State.Base

stateTToEnvOpsPipeline
  :: forall s m1 comp .
  (Monad m1, EffFunctor Lift comp)
  => SimplePipeline Lift (EnvEff s) (StateEff s) comp m1
stateTToEnvOpsPipeline = transformePipeline $ genericComputation handler
 where
  handler :: forall m
   . (EffConstraint (EnvEff s) m)
    => TransformerHandler (StateT s) (StateEff s) m
  handler = TransformerHandler stateTOps stateTLift $ Lift $
    \comp -> do
      i <- ask
      evalStateT comp i

stateTComp1 :: forall m . (Monad m)
  => BaseComputation (EnvEff Int) (Return ()) m
stateTComp1 = runPipeline
  stateTToEnvOpsPipeline stateBaseComp

stateTComp2 :: forall m . (Monad m)
  => BaseComputation NoEff (Return ()) (ReaderT Int m)
stateTComp2 = bindOpsHandler
  readerTHandler stateTComp1

stateToReaderComp :: forall m . (Monad m)
  => ReaderT Int m ()
stateToReaderComp = returnVal $ runComp stateTComp2 idLift NoOp

stateToReaderIdentityComp :: ReaderT Int Identity ()
stateToReaderIdentityComp = stateToReaderComp
