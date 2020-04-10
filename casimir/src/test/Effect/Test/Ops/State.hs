
module Effect.Test.Ops.State where

import Test.Tasty
import Test.Tasty.HUnit

import Data.IORef
import Control.Monad.Identity
import Control.Monad.Trans.State.Strict
  (StateT, runStateT, evalStateT)

import Casimir
import Casimir.Free

import Casimir.Ops.Io
import Casimir.Ops.Env
import Casimir.Ops.State
import Casimir.Ops.State.Lift
import Casimir.Ops.State.Free
import Casimir.Ops.State.Transform

stateTests :: TestTree
stateTests = testGroup "StateOps Tests"
  [ stateTHandlerTest
  , stateTToEnvOpsPipelineTest
  , ioStateTest
  , churchStateTest1
  , churchStateTest2
  , freeStateTest1
  ]

stateTHandler
  :: forall eff s .
  (Effect eff)
  => BaseOpsHandler NoEff (StateEff s) (StateT s eff)
stateTHandler = opsHandlerComp $
  \lifter -> applyLift lifter stateTOps

ioHandler :: BaseOpsHandler NoEff IoEff IO
ioHandler = baseOpsHandler IoOps {
  liftIoOp = id
}

stateTToEnvOpsPipeline
  :: forall s eff1 comp .
  (Effect eff1, EffFunctor comp)
  => SimplePipeline Lift (EnvEff s) (StateEff s) comp eff1
stateTToEnvOpsPipeline = transformePipeline $ genericComputation handler
 where
  handler :: forall eff
   . (EffConstraint (EnvEff s) eff)
    => TransformerHandler (StateT s) (StateEff s) eff
  handler = TransformerHandler stateTOps stateTLift $ Lift $
    \comp -> do
      i <- ask
      evalStateT comp i

type StateCompRes = (Int, Int, Int)

stateComp1 :: Eff (StateEff Int) StateCompRes
stateComp1 = do
  s1 <- get
  put $ s1 + 2
  s2 <- get
  s3 <- get
  return (s1, s2, s3)

stateComp2
  :: forall eff . (Effect eff)
  => BaseComputation (StateEff Int) (Return StateCompRes) eff
stateComp2 = genericReturn stateComp1

stateTComp
  :: forall eff . (Effect eff)
  => BaseComputation NoEff (Return StateCompRes) (StateT Int eff)
stateTComp = bindOpsHandler
  stateTHandler stateComp2

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

stateComp3 :: BaseComputation (EnvEff Int) (Return StateCompRes) Identity
stateComp3
  = runPipeline
    stateTToEnvOpsPipeline
    stateComp2

stateComp4 :: BaseComputation NoEff (Return StateCompRes) Identity
stateComp4 = bindOpsHandler
  (mkEnvHandler 4) stateComp3

stateTToEnvOpsPipelineTest :: TestTree
stateTToEnvOpsPipelineTest = testCase "StateT pipeline test" $
  assertEqual "StateT pipeline should get/put the correct states"
    (4, 6, 6) $ runIdentityComp stateComp4

ioStateHandler
  :: forall eff s .
  (Effect eff)
  => BaseOpsHandler (IoEff ∪ EnvEff (IORef s)) (StateEff s) eff
ioStateHandler = genericOpsHandler StateOps {
  getOp =
   do
    ref <- ask
    liftIo $ readIORef ref,

  putOp = \x ->
   do
    ref <- ask
    liftIo $ writeIORef ref x
}

ioStateComp :: IORef Int -> BaseComputation NoEff (Return StateCompRes) IO
ioStateComp ref =
  bindOpsHandler @NoEff
    ioHandler
    (bindOpsHandler @IoEff
      (mkEnvHandler ref)
      (bindOpsHandler
        @(IoEff ∪ EnvEff (IORef Int))
        ioStateHandler
        stateComp2
      ))

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

stateCoOpHandler
  :: forall eff s a .
  (Effect eff)
  => CoOpHandler (StateEff s) a (CoState s eff a) eff
stateCoOpHandler = CoOpHandler handleReturn handleOps
 where
  handleReturn :: a -> eff (CoState s eff a)
  handleReturn x = return $ CoState $ \_ -> return x

  handleOps :: StateCoOp s (eff (CoState s eff a)) -> eff (CoState s eff a)
  handleOps (GetOp cont1) = return $ CoState $
    \s ->
     do
      (CoState cont2) <- cont1 s
      cont2 s
  handleOps (PutOp s cont1) = return $ CoState $
    \_ ->
     do
      (CoState cont2) <- cont1 ()
      cont2 s

stateDynComp1
  :: forall eff .
  (Effect eff)
  => ChurchMonad (StateEff Int) eff StateCompRes
stateDynComp1 = withOps @(StateEff Int) freeOps stateComp1

stateDynComp2 :: forall eff . (Effect eff)
  => eff (CoState Int eff StateCompRes)
stateDynComp2 = withCoOpHandler @ChurchMonad stateCoOpHandler stateDynComp1

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
  => GenericPipeline Lift (EnvEff s) (StateEff s) eff1
statePipeline1 = contextualHandlerToPipeline @ChurchMonad $
  Computation handler
   where
    handler
      :: forall eff2 .
      (Effect eff2)
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

stateDynComp4 :: forall eff . (Effect eff)
  => BaseComputation (EnvEff Int) (Return StateCompRes) eff
stateDynComp4 = runPipeline
  statePipeline1 stateComp2

stateDynComp5 :: forall eff . (Effect eff)
  => BaseComputation NoEff (Return StateCompRes) eff
stateDynComp5 = bindOpsHandler
  (mkEnvHandler (6 :: Int))
  stateDynComp4

churchStateTest2 :: TestTree
churchStateTest2 = testCase "Church state test 2" $
  assertEqual "State ops pipeline should handle state correctly"
    (6, 8, 8) $
    runIdentityComp stateDynComp5

stateFreeComp1 :: forall eff . (Effect eff)
  => eff (CoState Int eff StateCompRes)
stateFreeComp1 = handleFree @FreeMonad stateCoOpHandler $
  withOps @(StateEff Int) freeOps stateComp1

stateFreeComp2 :: Identity StateCompRes
stateFreeComp2 = stateDynComp2 >>= runCoState 7

freeStateTest1 :: TestTree
freeStateTest1 = testCase "Free state test 1" $
 assertEqual "State ops handler should handle state correctly"
  (7, 9, 9) $
  runIdentity stateFreeComp2
