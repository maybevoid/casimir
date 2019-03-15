
module Effect.Test.Ops.State where

import Test.Tasty
import Test.Tasty.HUnit

import Data.IORef
import Control.Monad.Identity
-- import Control.Comonad.Store
-- import Control.Comonad.Cofree
import Control.Monad.Trans.State.Strict
  (StateT, runStateT)

import Control.Effect

stateTests :: TestTree
stateTests = testGroup "StateEff Tests"
  [ stateTHandlerTest
  , stateTPipelineTest
  , ioStateTest
  , churchStateTest1
  , churchStateTest2
  , freeStateTest1
  ]

type StateCompRes = (Int, Int, Int)

stateComp1
  :: forall eff .
  (Effect eff, OpsConstraint (StateEff Int) eff)
  => eff StateCompRes
stateComp1 = do
  s1 <- get
  put $ s1 + 2
  s2 <- get
  s3 <- get
  return (s1, s2, s3)

stateComp2 :: GenericReturn (StateEff Int) StateCompRes
stateComp2 = genericComputation $ Return stateComp1

stateTComp
  :: forall eff .
  (Effect eff)
  => Computation NoEff (Return StateCompRes) (StateT Int eff)
stateTComp = bindHandlerWithCast
  stateTHandler stateComp2
  cast cast

stateTHandlerTest :: TestTree
stateTHandlerTest = testCase "StateT handler test" $
 do
  let
    comp :: StateT Int Identity StateCompRes
      = execComp stateTComp

    (res, s) = runIdentity $ runStateT comp 3

  assertEqual "StateT computation should get/put the correct states"
    (3, 5, 5) res

  assertEqual "StateT computation should have 5 as final state"
    5 s

stateComp3 :: IdentityComputation StateCompRes
stateComp3
  = runPipelineWithCast
    (stateTPipeline 4)
    stateComp2
    cast cast

stateTPipelineTest :: TestTree
stateTPipelineTest = testCase "StateT pipeline test" $
  assertEqual "StateT pipeline should get/put the correct states"
    (4, 6, 6) $ runIdentityComp stateComp3

ioStateHandler
  :: forall eff s .
  (Effect eff)
  => FlatHandler (Union IoEff (EnvEff (IORef s))) (StateEff s) eff
ioStateHandler = genericHandler StateOps {
  getOp =
   do
    ref <- ask
    liftIo $ readIORef ref,

  putOp = \x ->
   do
    ref <- ask
    liftIo $ writeIORef ref x
}

ioStateComp :: IORef Int -> Computation NoEff (Return StateCompRes) IO
ioStateComp ref =
  bindHandlerWithCast @NoEff ioHandler
    (bindHandlerWithCast @IoEff (mkEnvHandler ref)
      (bindHandlerWithCast
        @(Union IoEff (EnvEff (IORef Int)))
        ioStateHandler
        stateComp2
        cast cast
      )
      cast cast)
    cast cast

ioStateTest :: TestTree
ioStateTest = testCase "IO State Test" $
 do
  ref <- newIORef 4
  res <- execComp $ ioStateComp ref

  assertEqual "IO State computation should get/put the correct states"
    (4, 6, 6) res

  s <- readIORef ref
  assertEqual "IO State  computation should have 6 as final state"
    6 s

newtype CoState s eff a = CoState (s -> eff a)

runCoState :: forall s eff . (Effect eff)
  => s
  -> (forall a . CoState s eff a -> eff a)
runCoState i (CoState cont) = cont i

stateOpsHandler
  :: forall eff s a .
  (Effect eff)
  => OpsHandler (StateEff s) a (CoState s eff a) eff
stateOpsHandler = OpsHandler handleReturn' handleOps'
 where
  handleReturn' :: a -> eff (CoState s eff a)
  handleReturn' x = return $ CoState $ \_ -> return x

  handleOps' :: StateCoOps s (eff (CoState s eff a)) -> eff (CoState s eff a)
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

stateDynComp1
  :: forall eff .
  (Effect eff)
  => ChurchMonad (StateEff Int) eff StateCompRes
stateDynComp1 = bindConstraint @(StateEff Int) churchOps stateComp1

stateDynComp2 :: forall eff . (Effect eff)
  => eff (CoState Int eff StateCompRes)
stateDynComp2 = withOpsHandler @ChurchMonad stateOpsHandler stateDynComp1

stateDynComp3 :: Identity StateCompRes
stateDynComp3 = stateDynComp2 >>= runCoState 5

churchStateTest1 :: TestTree
churchStateTest1 = testCase "Church state test 1" $
 assertEqual "State ops handler should handle state correctly"
  (5, 7, 7) $
  runIdentity stateDynComp3

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

stateDynComp4 :: forall eff . (Effect eff)
  => Computation (EnvEff Int) (Return StateCompRes) eff
stateDynComp4 = runPipelineWithCast
  statePipeline1 stateComp2
  cast cast

stateDynComp5 :: forall eff . (Effect eff)
  => Computation NoEff (Return StateCompRes) eff
stateDynComp5 = bindHandlerWithCast
  (mkEnvHandler (6 :: Int))
  stateDynComp4
  cast cast

churchStateTest2 :: TestTree
churchStateTest2 = testCase "Church state test 2" $
  assertEqual "State ops pipeline should handle state correctly"
    (6, 8, 8) $
    runIdentityComp stateDynComp5

stateFreeComp1 :: forall eff . (Effect eff)
  => eff (CoState Int eff StateCompRes)
stateFreeComp1 = handleFree @FreeMonad stateOpsHandler $
  bindConstraint @(StateEff Int) freeOps $ stateComp1

stateFreeComp2 :: Identity StateCompRes
stateFreeComp2 = stateDynComp2 >>= runCoState 7

freeStateTest1 :: TestTree
freeStateTest1 = testCase "Free state test 1" $
 assertEqual "State ops handler should handle state correctly"
  (7, 9, 9) $
  runIdentity stateFreeComp2

