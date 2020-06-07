
module Benchmark.State.FreePipeline
where

import Control.Monad.Identity
import Control.Monad.Trans.Reader (ReaderT)

import Casimir
import Casimir.Free
import Casimir.Ops.Env
import Casimir.Ops.State

import Benchmark.State.Base

{-# INLINE statePipeline1 #-}
statePipeline1
  :: forall free s eff1 .
  (Monad eff1, FreeHandler free)
  => GenericPipeline Lift (EnvEff s) (StateEff s) eff1
statePipeline1 = contextualHandlerToPipeline @free $
  Computation handler
   where
    handler
      :: forall eff2 .
      (Monad eff2)
      => Lift eff1 eff2
      -> EnvOps s eff2
      -> ContextualHandler (CoState s) (StateEff s) eff2
    handler _ envOps = ContextualHandler coopHandler extract
     where
      coopHandler :: forall a .
        CoOpHandler (StateEff s) a (CoState s eff2 a) eff2
      coopHandler = stateCoOpHandler

      extract :: forall a . CoState s eff2 a -> eff2 a
      extract (CoState cont) = withOps envOps $
       do
        s <- ask
        cont s

stateFreeComp1
  :: forall free eff .
  (FreeHandler free, Monad eff)
  => BaseComputation (EnvEff Int) (Return ()) eff
stateFreeComp1 = runPipeline
  (statePipeline1 @free) stateBaseComp

stateFreeComp2
  :: forall free eff .
  (FreeHandler free, Monad eff)
  => BaseComputation NoEff (Return ()) (ReaderT Int eff)
stateFreeComp2 = bindOpsHandler
  readerTHandler
  (stateFreeComp1 @free)

readerTFreeComp
  :: forall free eff .
  (FreeHandler free, Monad eff)
  => ReaderT Int eff ()
readerTFreeComp = returnVal $ runComp (stateFreeComp2 @free) idLift NoOp

curriedFreeComp
  :: forall free eff .
  (FreeHandler free, Monad eff)
  => Int
  -> BaseComputation NoEff (Return ()) eff
curriedFreeComp s = bindOpsHandler
  (mkEnvHandler s)
  (stateFreeComp1 @free)

readerTFreeIdentityComp
  :: forall free .
  (FreeHandler free)
  => ReaderT Int Identity ()
readerTFreeIdentityComp = readerTFreeComp @free

curriedFreeIdentityComp
  :: forall free .
  (FreeHandler free)
  => Int
  -> BaseComputation NoEff (Return ()) Identity
curriedFreeIdentityComp = curriedFreeComp @free
