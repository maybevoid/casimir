{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where
import Criterion.Main

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)

import qualified Control.Monad.Trans.Reader as RT

import Control.Effect

newtype CoState s eff a = CoState (s -> eff a)

runCoState :: forall s eff . (Effect eff)
  => s
  -> (forall a . CoState s eff a -> eff a)
runCoState i (CoState cont) = cont i

stateComp1
  :: forall eff
   . (Effect eff, OpsConstraint (StateEff Int) eff)
  => eff ()
stateComp1 = forM_ [0..500] $ \i ->
 do
  s <- get
  let s' = s + i
  put s'

stateComp2 :: GenericReturn (StateEff Int) ()
stateComp2 = genericReturn stateComp1

stateHComp1 :: Computation NoEff (Return ()) (StateT Int Identity)
stateHComp1 = bindHandlerWithCast
  stateTHandler stateComp2
  cast cast

stateHComp2 :: StateT Int Identity ()
stateHComp2 = returnVal $ runComp stateHComp1 idLift NoOp

stateOpsHandler
  :: forall eff s a .
  (Effect eff)
  => OpsHandler (StateEff s) a (CoState s eff a) eff
stateOpsHandler = OpsHandler handleReturn' handleOps'
 where
  handleReturn' :: a -> eff (CoState s eff a)
  handleReturn' x = return $ CoState $ \_ -> return x

  handleOps' :: StateCoOp s (eff (CoState s eff a)) -> eff (CoState s eff a)
  handleOps' (GetOp cont1) = return $ CoState $
    \s ->
     do
      (CoState cont2) <- cont1 s
      cont2 s
  handleOps' (PutOp s cont1) = return $ CoState $
    \_ ->
     do
      (CoState cont2) <- cont1 ()
      cont2 s

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
      opsHandler = stateOpsHandler

      extract :: forall a . CoState s eff2 a -> eff2 a
      extract (CoState cont) = bindConstraint envOps $
       do
        s <- ask
        cont s

stateTComp1 :: StateT Int Identity ()
stateTComp1 = withHandler stateTHandler stateComp1

stateTComp12 :: ReaderT Int Identity ()
stateTComp12 = do
  s <- RT.ask
  lift $ evalStateT stateTComp1 s

stateTComp2 :: forall eff . (Effect eff)
  => Computation (EnvEff Int) (Return ()) eff
stateTComp2 = runPipelineWithCast
  stateTPipeline stateComp2
  cast cast

stateTComp3 :: forall eff . (Effect eff)
  => Computation NoEff (Return ()) (ReaderT Int eff)
stateTComp3 = bindHandlerWithCast
  readerTHandler stateTComp2
  cast cast

stateTComp4 :: ReaderT Int Identity ()
stateTComp4 = returnVal $ runComp stateTComp3 idLift NoOp

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

{-# INLINE stateFreeComp1 #-}
stateFreeComp1 :: forall free . (FreeEff free)
  => Computation (EnvEff Int) (Return ()) Identity
stateFreeComp1 = runPipelineWithCast
  (statePipeline1 @free) stateComp2
  cast cast

{-# INLINE stateFreeComp2 #-}
stateFreeComp2 :: forall free . (FreeEff free)
  => Computation NoEff (Return ()) (ReaderT Int Identity)
stateFreeComp2 = bindHandlerWithCast
  readerTHandler
  (stateFreeComp1 @free)
  cast cast

{-# INLINE stateFreeComp3 #-}
stateFreeComp3 :: forall free . (FreeEff free)
  => ReaderT Int Identity ()
stateFreeComp3 = returnVal $ runComp (stateFreeComp2 @free) idLift NoOp

-- runStateComp
--   :: (Int -> Computation NoEff (Return a) Identity)
--   -> a
-- runStateComp comp = runIdentity $ returnVal $
--   runComp (comp 5) id NoOp

statePComp1 :: Computation (EnvEff Int) (Return ()) Identity
statePComp1 = statePipeline2 stateComp2

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
statePComp4 = statePipeline3 stateComp2

statePComp5 :: ReaderT Int Identity ()
statePComp5 = returnVal $ runComp statePComp4 idLift NoOp

main :: IO ()
main = defaultMain [
  bgroup "state benchmark"
    [ bench "StateT Handler"  $
        nf (\m -> runIdentity $ evalStateT m 5) stateTComp1
    , bench "StateT Computation"  $
        nf (\m -> runIdentity $ evalStateT m 5) stateHComp2
    , bench "ReaderT StateT Handler" $
        nf (\m -> runIdentity $ runReaderT m 5) stateTComp12
    , bench "Direct ReaderT StateT Handler"  $
        nf (\m -> runIdentity $ runReaderT m 5) statePComp3
    , bench "Fused ReaderT StateT Handler"  $
        nf (\m -> runIdentity $ runReaderT m 5) statePComp5
    , bench "StateT Pipeline" $
        nf (\m -> runIdentity $ runReaderT m 5) stateTComp4
    -- , bench "StateT Pipeline 2"  $
    --     nf (\comp -> runIdentity $ comp 5) stateTComp3
    , bench "FreeMonad"  $
        nf (\m -> runIdentity $ runReaderT m 5) (stateFreeComp3 @FreeMonad)
    , bench "FreerMonad"  $
        nf (\m -> runIdentity $ runReaderT m 5) (stateFreeComp3 @FreerMonad)
    , bench "ChurchMonad"  $
        nf (\m -> runIdentity $ runReaderT m 5) (stateFreeComp3 @ChurchMonad)
    -- , bench "FreerMonad"  $
    --     nf runStateComp (stateFreeComp @FreerMonad)
    -- , bench "ChurchMonad"  $
    --     nf runStateComp (stateFreeComp @ChurchMonad)
    ]
  ]