
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
    handler _ envOps = ContextualHandler opsHandler extract
     where
      opsHandler :: forall a .
        OpsHandler (StateEff s) a (CoState s eff2 a) eff2
      opsHandler = stateCoOpHandler

      extract :: forall a . CoState s eff2 a -> eff2 a
      extract (CoState cont) = bindConstraint envOps $
       do
        s <- ask
        cont s

stateFreeComp1 :: forall free . (FreeEff free)
  => Computation (EnvEff Int) (Return ()) Identity
stateFreeComp1 = runPipelineWithCast
  (statePipeline1 @free) stateBaseComp
  cast cast

stateFreeComp2 :: forall free . (FreeEff free)
  => Computation NoEff (Return ()) (ReaderT Int Identity)
stateFreeComp2 = bindHandlerWithCast
  readerTHandler
  (stateFreeComp1 @free)
  cast cast

readerTFreeComp :: forall free . (FreeEff free)
  => ReaderT Int Identity ()
readerTFreeComp = returnVal $ runComp (stateFreeComp2 @free) idLift NoOp

curriedFreeComp:: forall free . (FreeEff free)
  => Int
  -> Computation NoEff (Return ()) Identity
curriedFreeComp s = bindHandlerWithCast
  (mkEnvHandler s)
  (stateFreeComp1 @free)
  cast cast

applyCurriedComp
  :: (Int -> Computation NoEff (Return a) Identity)
  -> a
applyCurriedComp comp = runIdentity $ returnVal $
  runComp (comp 5) idLift NoOp
