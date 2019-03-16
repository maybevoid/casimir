{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where
import Criterion.Main

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.State.Strict (StateT, evalStateT)

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
  :: forall free s eff1 .
  (Effect eff1, FreeEff free)
  => s
  -> GenericPipeline NoEff (StateEff s) eff1
statePipeline1 s = contextualHandlerToPipeline @free $
  Computation handler
   where
    handler
      :: forall eff2 .
      (Effect eff2)
      => eff1 ~> eff2
      -> Operation NoEff eff2
      -> ContextualHandler (CoState s) (StateEff s) eff2
    handler _ _ = ContextualHandler opsHandler extract
     where
      opsHandler :: forall a .
        OpsHandler (StateEff s) a (CoState s eff2 a) eff2
      opsHandler = stateOpsHandler

      extract :: forall a . CoState s eff2 a -> eff2 a
      extract (CoState cont) = cont s

stateTComp1 :: StateT Int Identity ()
stateTComp1 = withHandler stateTHandler stateComp1

stateTComp2 :: IdentityComputation ()
stateTComp2 = runPipelineWithCast
  (stateTPipeline 5) stateComp2
  cast cast

stateFreeComp :: forall free . (FreeEff free)
  => IdentityComputation ()
stateFreeComp = runPipelineWithCast
  (statePipeline1 @free 5) stateComp2
  cast cast

main :: IO ()
main = defaultMain [
  bgroup "state benchmark"
    [ bench "StateT Handler"  $
        nf (\m -> runIdentity $ evalStateT m 5) stateTComp1
    , bench "StateT Pipeline"  $
        nf runIdentityComp stateTComp2
    , bench "FreeMonad"  $
        nf runIdentityComp (stateFreeComp @FreeMonad)
    , bench "FreerMonad"  $
        nf runIdentityComp (stateFreeComp @FreerMonad)
    , bench "ChurchMonad"  $
        nf runIdentityComp (stateFreeComp @ChurchMonad)
    ]
  ]