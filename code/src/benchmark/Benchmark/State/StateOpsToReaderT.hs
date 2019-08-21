
module Benchmark.State.StateOpsToReaderT
  (stateOpsToReaderTComp)
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)

import qualified Control.Monad.Trans.Reader as RT

import Control.Effect.Implicit
import Control.Effect.Implicit.Ops.State
import Control.Effect.Implicit.Transform.State

import Benchmark.State.Base

stateOpsToReaderTPipeline
  :: forall s a eff1
   . (Effect eff1)
  => Computation (StateOps s) (Return a) eff1
  -> Computation NoOp (Return a) (ReaderT s eff1)
stateOpsToReaderTPipeline comp1 = Computation comp2
 where
  comp2 :: forall eff2 . (Effect eff2)
    => LiftEff (ReaderT s eff1) eff2
    -> NoOp eff2
    -> Return a eff2
  comp2 lift12 _ = Return $ liftEff lift12 comp5

  comp3 :: Computation NoOp (Return a) (StateT s eff1)
  comp3 = bindOpsHandler
    stateTHandler
    (liftComputation stateTLiftEff comp1)

  comp4 :: StateT s eff1 a
  comp4 = returnVal $ runComp comp3 idLift NoOp

  comp5 :: ReaderT s eff1 a
  comp5 = do
    s <- RT.ask
    lift $ evalStateT comp4 s

stateComp1
  :: forall eff . (Effect eff)
  => Computation NoOp (Return ()) (ReaderT Int eff)
stateComp1 = stateOpsToReaderTPipeline stateBaseComp

stateOpsToReaderTComp
  :: forall eff . (Effect eff)
  => ReaderT Int eff ()
stateOpsToReaderTComp = returnVal $ runComp stateComp1 idLift NoOp
