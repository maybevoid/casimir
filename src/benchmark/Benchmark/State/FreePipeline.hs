
module Benchmark.State.FreePipeline
where

import Control.Monad.Identity
import Control.Monad.Trans.Reader (ReaderT)

import Control.Effect
import Benchmark.State.Base

{-# INLINE statePipeline1 #-}
statePipeline1
  :: forall free s eff1 .
  (Effect eff1, FreeEff free)
  => GenericPipeline (EnvEff s) (StateEff s) eff1
statePipeline1 = contextualHandlerToPipeline @free $
  Computation handler
   where
    handler
      :: forall eff2 .
      (Effect eff2)
      => LiftEff eff1 eff2
      -> Operation (EnvEff s) eff2
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
  (FreeEff free, Effect eff)
  => Computation (EnvEff Int) (Return ()) eff
stateFreeComp1 = runPipelineWithCast
  (statePipeline1 @free) stateBaseComp
  cast cast

stateFreeComp2
  :: forall free eff .
  (FreeEff free, Effect eff)
  => Computation NoEff (Return ()) (ReaderT Int eff)
stateFreeComp2 = bindHandlerWithCast
  readerTHandler
  (stateFreeComp1 @free)
  cast cast

readerTFreeComp
  :: forall free eff .
  (FreeEff free, Effect eff)
  => ReaderT Int eff ()
readerTFreeComp = returnVal $ runComp (stateFreeComp2 @free) idLift NoOp

curriedFreeComp
  :: forall free eff .
  (FreeEff free, Effect eff)
  => Int
  -> Computation NoEff (Return ()) eff
curriedFreeComp s = bindHandlerWithCast
  (mkEnvHandler s)
  (stateFreeComp1 @free)
  cast cast

readerTFreeIdentityComp
  :: forall free .
  (FreeEff free)
  => ReaderT Int Identity ()
readerTFreeIdentityComp = readerTFreeComp @free

curriedFreeIdentityComp
  :: forall free .
  (FreeEff free)
  => Int
  -> Computation NoEff (Return ()) Identity
curriedFreeIdentityComp = curriedFreeComp @free
