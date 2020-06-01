
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
  :: forall s a m1
   . (Monad m1)
  => BaseComputation (StateEff s) (Return a) m1
  -> BaseComputation NoEff (Return a) (ReaderT s m1)
stateOpsToReaderTPipeline comp1 = Computation comp2
 where
  comp2 :: forall m2 . (Monad m2)
    => Lift (ReaderT s m1) m2
    -> NoOp m2
    -> Return a m2
  comp2 lift12 _ = Return $ runLift lift12 comp5

  comp3 :: BaseComputation NoEff (Return a) (StateT s m1)
  comp3 = bindOpsHandler
    stateTHandler
    (liftComputation stateTLift comp1)

  comp4 :: StateT s m1 a
  comp4 = returnVal $ runComp comp3 idLift NoOp

  comp5 :: ReaderT s m1 a
  comp5 = do
    s <- RT.ask
    lift $ evalStateT comp4 s

stateComp1
  :: forall m . (Monad m)
  => BaseComputation NoEff (Return ()) (ReaderT Int m)
stateComp1 = stateOpsToReaderTPipeline stateBaseComp

stateOpsToReaderTComp
  :: forall m . (Monad m)
  => ReaderT Int m ()
stateOpsToReaderTComp = returnVal $ runComp stateComp1 idLift NoOp
