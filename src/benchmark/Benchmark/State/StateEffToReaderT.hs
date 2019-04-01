
module Benchmark.State.StateEffToReaderT
  (stateEffToReaderTComp)
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)

import qualified Control.Monad.Trans.Reader as RT

import Control.Effect.Implicit
import Control.Effect.Implicit.Ops.State
import Control.Effect.Implicit.Transform.State

import Benchmark.State.Base

stateEffToReaderTPipeline
  :: forall s a eff1
   . (Effect eff1)
  => Computation (StateEff s) (Return a) eff1
  -> Computation NoEff (Return a) (ReaderT s eff1)
stateEffToReaderTPipeline comp1 = Computation comp2
 where
  comp2 :: forall eff2 . (Effect eff2)
    => LiftEff (ReaderT s eff1) eff2
    -> Operation NoEff eff2
    -> Return a eff2
  comp2 lift12 _ = Return $ liftEff lift12 comp5

  comp3 :: Computation NoEff (Return a) (StateT s eff1)
  comp3 = bindOpsHandlerWithCast
    cast cast
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
  => Computation NoEff (Return ()) (ReaderT Int eff)
stateComp1 = stateEffToReaderTPipeline stateBaseComp

stateEffToReaderTComp
  :: forall eff . (Effect eff)
  => ReaderT Int eff ()
stateEffToReaderTComp = returnVal $ runComp stateComp1 idLift NoOp
