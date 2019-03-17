{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where
import Criterion.Main

import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)

import qualified Control.Monad.Trans.Reader as RT

import Control.Effect

import Benchmark.State

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

{-# INLINE statePipeline2 #-}
statePipeline2
  :: forall s a eff1
   . (Effect eff1)
  => Computation (StateEff s) (Return a) eff1
  -> Computation (EnvEff s) (Return a) eff1
statePipeline2 comp1 = Computation comp2
 where
  comp2 :: forall eff2 . (Effect eff2)
    => LiftEff eff1 eff2
    -> Operation (EnvEff s) eff2
    -> Return a eff2
  comp2 lift12 ops = bindConstraint ops $ Return comp5
   where
    comp3 :: Computation NoEff (Return a) (StateT s eff2)
    comp3 = bindHandlerWithCast
      stateTHandler
      (liftComputation lift12 comp1)
      cast cast

    comp4 :: StateT s eff2 a
    comp4 = returnVal $ runComp comp3 idLift NoOp

    comp5 :: (OpsConstraint (EnvEff s) eff2) => eff2 a
    comp5 = do
      s <- ask
      res <- evalStateT comp4 s
      return res

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

stateFreeComp3 :: forall free . (FreeEff free)
  => ReaderT Int Identity ()
stateFreeComp3 = returnVal $ runComp (stateFreeComp2 @free) idLift NoOp

stateFreeComp4:: forall free . (FreeEff free)
  => Int
  -> Computation NoEff (Return ()) Identity
stateFreeComp4 s = bindHandlerWithCast
  (mkEnvHandler s)
  (stateFreeComp1 @free)
  cast cast

runStateComp
  :: (Int -> Computation NoEff (Return a) Identity)
  -> a
runStateComp comp = runIdentity $ returnVal $
  runComp (comp 5) idLift NoOp

statePComp1 :: Computation (EnvEff Int) (Return ()) Identity
statePComp1 = statePipeline2 stateBaseComp

statePComp2 :: Computation NoEff (Return ()) (ReaderT Int Identity)
statePComp2 = bindHandlerWithCast
  readerTHandler statePComp1
  cast cast

statePComp3 :: ReaderT Int Identity ()
statePComp3 = returnVal $ runComp statePComp2 idLift NoOp

{-# INLINE statePipeline3 #-}
statePipeline3
  :: forall s a eff1
   . (Effect eff1)
  => Computation (StateEff s) (Return a) eff1
  -> Computation NoEff (Return a) (ReaderT s eff1)
statePipeline3 comp1 = Computation comp2
 where
  comp2 :: forall eff2 . (Effect eff2)
    => LiftEff (ReaderT s eff1) eff2
    -> Operation NoEff eff2
    -> Return a eff2
  comp2 lift12 _ = Return $ liftEff lift12 comp5

  comp3 :: Computation NoEff (Return a) (StateT s eff1)
  comp3 = bindHandlerWithCast
    stateTHandler
    comp1
    cast cast

  comp4 :: StateT s eff1 a
  comp4 = returnVal $ runComp comp3 idLift NoOp

  comp5 :: ReaderT s eff1 a
  comp5 = do
    s <- RT.ask
    res <- lift $ evalStateT comp4 s
    return res

statePComp4 :: Computation NoEff (Return ()) (ReaderT Int Identity)
statePComp4 = statePipeline3 stateBaseComp

statePComp5 :: ReaderT Int Identity ()
statePComp5 = returnVal $ runComp statePComp4 idLift NoOp

main :: IO ()
main = defaultMain [
  bgroup "state benchmark"
    [ bench "StateT Handler"  $
        nf (\m -> runIdentity $ evalStateT m 5) stateTComp1
    , bench "With StateT Computation" $
        nf (\m -> runIdentity $ runReaderT m 5) withStateTComp
    , bench "StateT Computation"  $
        nf (\m -> runIdentity $ evalStateT m 5) stateTHandlerComp
    -- , bench "Direct ReaderT StateT Handler"  $
    --     nf (\m -> runIdentity $ runReaderT m 5) statePComp3
    -- , bench "Fused ReaderT StateT Handler"  $
    --     nf (\m -> runIdentity $ runReaderT m 5) statePComp5
    , bench "StateT Pipeline" $
        nf (\m -> runIdentity $ runReaderT m 5) stateToReaderComp
    , bench "ReaderT FreeMonad"  $
        nf (\m -> runIdentity $ runReaderT m 5) (stateFreeComp3 @FreeMonad)
    , bench "ReaderT FreerMonad"  $
        nf (\m -> runIdentity $ runReaderT m 5) (stateFreeComp3 @FreerMonad)
    , bench "ReaderT ChurchMonad"  $
        nf (\m -> runIdentity $ runReaderT m 5) (stateFreeComp3 @ChurchMonad)
    -- , bench "Curried FreeMonad"  $
    --     nf runStateComp (stateFreeComp4 @FreeMonad)
    -- , bench "Curried FreerMonad"  $
    --     nf runStateComp (stateFreeComp4 @FreerMonad)
    -- , bench "Curried ChurchMonad"  $
    --     nf runStateComp (stateFreeComp4 @ChurchMonad)
    ]
  ]