
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
  :: forall free s m1 .
  (Monad m1, FreeHandler free)
  => GenericPipeline Lift (EnvEff s) (StateEff s) m1
statePipeline1 = contextualHandlerToPipeline @free $
  Computation handler
   where
    handler
      :: forall m2 .
      (Monad m2)
      => Lift m1 m2
      -> EnvOps s m2
      -> ContextualHandler (CoState s) (StateEff s) m2
    handler _ envOps = ContextualHandler coopHandler extract
     where
      coopHandler :: forall a .
        CoOpHandler (StateEff s) a (CoState s m2 a) m2
      coopHandler = stateCoOpHandler

      extract :: forall a . CoState s m2 a -> m2 a
      extract (CoState cont) = withOps envOps $
       do
        s <- ask
        cont s

stateFreeComp1
  :: forall free m .
  (FreeHandler free, Monad m)
  => BaseComputation (EnvEff Int) (Return ()) m
stateFreeComp1 = runPipeline
  (statePipeline1 @free) stateBaseComp

stateFreeComp2
  :: forall free m .
  (FreeHandler free, Monad m)
  => BaseComputation NoEff (Return ()) (ReaderT Int m)
stateFreeComp2 = bindOpsHandler
  readerTHandler
  (stateFreeComp1 @free)

readerTFreeComp
  :: forall free m .
  (FreeHandler free, Monad m)
  => ReaderT Int m ()
readerTFreeComp = returnVal $ runComp (stateFreeComp2 @free) idLift NoOp

curriedFreeComp
  :: forall free m .
  (FreeHandler free, Monad m)
  => Int
  -> BaseComputation NoEff (Return ()) m
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
