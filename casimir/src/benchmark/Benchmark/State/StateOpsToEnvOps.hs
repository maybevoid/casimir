
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
  :: forall s a eff1
   . (Effect eff1)
  => BaseComputation (StateEff s) (Return a) eff1
  -> BaseComputation (EnvEff s) (Return a) eff1
stateOpsToEnvOpsPipeline comp1 = Computation comp2
 where
  comp2 :: forall eff2 . (Effect eff2)
    => Lift eff1 eff2
    -> EnvOps s eff2
    -> Return a eff2
  comp2 lift12 ops = withOps ops $ Return comp5
   where
    comp3 :: BaseComputation NoEff (Return a) (StateT s eff2)
    comp3 = bindOpsHandler
      stateTHandler
      (liftComputation (joinLift lift12 stateTLift) comp1)

    comp4 :: StateT s eff2 a
    comp4 = returnVal $ runComp comp3 idLift NoOp

    comp5 :: (OpsConstraint (EnvEff s) eff2) => eff2 a
    comp5 = do
      s <- ask
      evalStateT comp4 s

statePComp1
  :: forall eff . (Effect eff)
  => BaseComputation (EnvEff Int) (Return ()) eff
statePComp1 = stateOpsToEnvOpsPipeline stateBaseComp

statePComp2
  :: forall eff . (Effect eff)
  => BaseComputation NoEff (Return ()) (ReaderT Int eff)
statePComp2 = bindOpsHandler
  readerTHandler statePComp1

stateOpsToEnvOpsToReaderTComp
  :: forall eff . (Effect eff)
  => ReaderT Int eff ()
stateOpsToEnvOpsToReaderTComp = returnVal $ runComp statePComp2 idLift NoOp

stateOpsToEnvOpsToReaderTIdentityComp
  :: ReaderT Int Identity ()
stateOpsToEnvOpsToReaderTIdentityComp = stateOpsToEnvOpsToReaderTComp
