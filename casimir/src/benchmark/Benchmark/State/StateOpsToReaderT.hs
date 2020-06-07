
module Benchmark.State.StateOpsToReaderT
  (stateOpsToReaderTComp)
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)

import qualified Control.Monad.Trans.Reader as RT

import Casimir
import Casimir.Ops.State
import Casimir.Ops.State.Lift

import Benchmark.State.Base

stateOpsToReaderTPipeline
  :: forall s a eff1
   . (Monad eff1)
  => BaseComputation (StateEff s) (Return a) eff1
  -> BaseComputation NoEff (Return a) (ReaderT s eff1)
stateOpsToReaderTPipeline comp1 = Computation comp2
 where
  comp2 :: forall eff2 . (Monad eff2)
    => Lift (ReaderT s eff1) eff2
    -> NoOp eff2
    -> Return a eff2
  comp2 lift12 _ = Return $ runLift lift12 comp5

  comp3 :: BaseComputation NoEff (Return a) (StateT s eff1)
  comp3 = bindOpsHandler
    stateTHandler
    (liftComputation stateTLift comp1)

  comp4 :: StateT s eff1 a
  comp4 = returnVal $ runComp comp3 idLift NoOp

  comp5 :: ReaderT s eff1 a
  comp5 = do
    s <- RT.ask
    lift $ evalStateT comp4 s

stateComp1
  :: forall eff . (Monad eff)
  => BaseComputation NoEff (Return ()) (ReaderT Int eff)
stateComp1 = stateOpsToReaderTPipeline stateBaseComp

stateOpsToReaderTComp
  :: forall eff . (Monad eff)
  => ReaderT Int eff ()
stateOpsToReaderTComp = returnVal $ runComp stateComp1 idLift NoOp
