
module Benchmark.State.StateOpsToEnvOps
  ( stateOpsToEnvOpsToReaderTComp

  -- The simple action of exporting a specialized
  -- Identity base monad improves performance by ~25%
  , stateOpsToEnvOpsToReaderTIdentityComp
  )
where

import Control.Monad.Identity
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)

import Casimir
import Casimir.Ops.Env
import Casimir.Ops.State
import Casimir.Ops.State.Lift

import Benchmark.State.Base

stateOpsToEnvOpsPipeline
  :: forall s a m1
   . (Monad m1)
  => BaseComputation (StateEff s) (Return a) m1
  -> BaseComputation (EnvEff s) (Return a) m1
stateOpsToEnvOpsPipeline comp1 = Computation comp2
 where
  comp2 :: forall m2 . (Monad m2)
    => Lift m1 m2
    -> EnvOps s m2
    -> Return a m2
  comp2 lift12 ops = withOps ops $ Return comp5
   where
    comp3 :: BaseComputation NoEff (Return a) (StateT s m2)
    comp3 = bindOpsHandler
      stateTHandler
      (liftComputation (joinLift lift12 stateTLift) comp1)

    comp4 :: StateT s m2 a
    comp4 = returnVal $ runComp comp3 idLift NoOp

    comp5 :: (OpsConstraint (EnvEff s) m2) => m2 a
    comp5 = do
      s <- ask
      evalStateT comp4 s

statePComp1
  :: forall m . (Monad m)
  => BaseComputation (EnvEff Int) (Return ()) m
statePComp1 = stateOpsToEnvOpsPipeline stateBaseComp

statePComp2
  :: forall m . (Monad m)
  => BaseComputation NoEff (Return ()) (ReaderT Int m)
statePComp2 = bindOpsHandler
  readerTHandler statePComp1

stateOpsToEnvOpsToReaderTComp
  :: forall m . (Monad m)
  => ReaderT Int m ()
stateOpsToEnvOpsToReaderTComp = returnVal $ runComp statePComp2 idLift NoOp

stateOpsToEnvOpsToReaderTIdentityComp
  :: ReaderT Int Identity ()
stateOpsToEnvOpsToReaderTIdentityComp = stateOpsToEnvOpsToReaderTComp
