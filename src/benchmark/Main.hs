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

statePipeline1
  :: forall s eff1 .
  (Effect eff1)
  => GenericPipeline (EnvEff s) (StateEff s) eff1
statePipeline1 = contextualHandlerToPipeline @ChurchMonad $
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

-- stateFreeComp :: forall free . (FreeEff free)
--   => Int
--   -> Computation NoEff (Return ()) Identity
-- stateFreeComp s = runPipelineWithCast
--   (statePipeline1 @free s) stateComp2
--   cast cast

-- runStateComp
--   :: (Int -> Computation NoEff (Return a) Identity)
--   -> a
-- runStateComp comp = runIdentity $ returnVal $
--   runComp (comp 5) id NoOp

-- statePipeline2
--   :: forall s a eff1
--    . (Effect eff1)
--   => Computation (StateEff s) (Return a) eff1
--   -> Computation (EnvEff s) (Return a) eff1
-- statePipeline2 comp1 = Computation comp2
--  where
--   comp2 :: forall eff2 . (Effect eff2)
--     => eff1 ~> eff2
--     -> Operation (EnvEff s) eff2
--     -> Return a eff2
--   comp2 liftEff ops = bindConstraint ops $ do
--     s <- RT.ask
--     res <- evalStateT returnVal $ comp4
--     return $ Return res

--    where
--     comp4 :: Return a (StateT s eff2)
--     comp4 = runComp comp3 liftEff NoOp

--   comp3 :: Computation NoEff (Return a) (StateT s eff1)
--   comp3 = bindHandlerWithCast
--     stateTHandler comp1
--     cast cast

main :: IO ()
main = defaultMain [
  bgroup "state benchmark"
    [ bench "StateT Handler"  $
        nf (\m -> runIdentity $ evalStateT m 5) stateTComp1
    , bench "ReaderT StateT Handler"  $
        nf (\m -> runIdentity $ runReaderT m 5) stateTComp12
    , bench "StateT Pipeline" $
        nf (\m -> runIdentity $ runReaderT m 5) stateTComp4
    -- , bench "StateT Pipeline 2"  $
    --     nf (\comp -> runIdentity $ comp 5) stateTComp3
    -- , bench "FreeMonad"  $
    --     nf runStateComp (stateFreeComp @FreeMonad)
    -- , bench "FreerMonad"  $
    --     nf runStateComp (stateFreeComp @FreerMonad)
    -- , bench "ChurchMonad"  $
    --     nf runStateComp (stateFreeComp @ChurchMonad)
    ]
  ]