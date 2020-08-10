
module Casimir.Test.Ops.State where

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
  :: forall m s .
  (Monad m)
  => BaseOpsHandler Nil (State s) (StateT s m)
stateTHandler = opsHandlerComp $
  \lifter -> effmap lifter stateTOps

ioHandler :: BaseOpsHandler Nil IoEff IO
ioHandler = baseOpsHandler IoOps {
  liftIoOp = id
}

stateTToEnvOpsPipeline
  :: forall s m1 comp .
  ( Monad m1
  , EffFunctor Lift comp
  )
  => SimplePipeline Lift (EnvEff s) (State s) comp m1
stateTToEnvOpsPipeline = transformePipeline $ genericComputation handler
 where
  handler :: forall m
   . (EffConstraint (EnvEff s) m)
    => TransformerHandler (StateT s) (State s) m
  handler = TransformerHandler stateTOps stateTLift $ Lift $
    \comp -> do
      i <- ask
      evalStateT comp i

type StateCompRes = (Int, Int, Int)

stateComp1 :: Eff (State Int) StateCompRes
stateComp1 = do
  s1 <- get
  put $ s1 + 2
  s2 <- get
  s3 <- get
  return (s1, s2, s3)

stateComp2
  :: forall m . (Monad m)
  => BaseComputation (State Int) (Return StateCompRes) m
stateComp2 = genericReturn stateComp1

stateTComp
  :: forall m . (Monad m)
  => BaseComputation Nil (Return StateCompRes) (StateT Int m)
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

stateComp4 :: BaseComputation Nil (Return StateCompRes) Identity
stateComp4 = bindOpsHandler
  (mkEnvHandler 4) stateComp3

stateTToEnvOpsPipelineTest :: TestTree
stateTToEnvOpsPipelineTest = testCase "StateT pipeline test" $
  assertEqual "StateT pipeline should get/put the correct states"
    (4, 6, 6) $ runIdentityComp stateComp4

ioStateHandler
  :: forall m s .
  (Monad m)
  => BaseOpsHandler (IoEff ∪ EnvEff (IORef s)) (State s) m
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

ioStateComp :: IORef Int -> BaseComputation Nil (Return StateCompRes) IO
ioStateComp ref =
  bindOpsHandler @Nil
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

newtype CoState s m a = CoState (s -> m a)

runCoState :: forall s m . (Monad m)
  => s
  -> (forall a . CoState s m a -> m a)
runCoState i (CoState cont) = cont i

stateCoOpHandler
  :: forall m s a .
  (Monad m)
  => CoOpHandler (State s) a (CoState s m a) m
stateCoOpHandler = CoOpHandler handleReturn handleOps
 where
  handleReturn :: a -> m (CoState s m a)
  handleReturn x = return $ CoState $ \_ -> return x

  handleOps :: StateCoOp s (m (CoState s m a)) -> m (CoState s m a)
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
  :: forall m .
  (Monad m)
  => ChurchMonad (State Int) m StateCompRes
stateDynComp1 = withOps @(State Int) freeOps stateComp1

stateDynComp2 :: forall m . (Monad m)
  => m (CoState Int m StateCompRes)
stateDynComp2 = withCoOpHandler @ChurchMonad stateCoOpHandler stateDynComp1

stateDynComp3 :: Identity StateCompRes
stateDynComp3 = stateDynComp2 >>= runCoState 5

churchStateTest1 :: TestTree
churchStateTest1 = testCase "Church state test 1" $
 assertEqual "State ops handler should handle state correctly"
  (5, 7, 7) $
  runIdentity stateDynComp3

statePipeline1
  :: forall s m1 .
  (Monad m1)
  => GenericPipeline Lift (EnvEff s) (State s) m1
statePipeline1 = contextualHandlerToPipeline @ChurchMonad $
  Computation handler
   where
    handler
      :: forall m2 .
      (Monad m2)
      => Lift m1 m2
      -> EnvOps s m2
      -> ContextualHandler (CoState s) (State s) m2
    handler _ envOps = ContextualHandler coopHandler extract
     where
      coopHandler :: forall a .
        CoOpHandler (State s) a (CoState s m2 a) m2
      coopHandler = stateCoOpHandler

      extract :: forall a . CoState s m2 a -> m2 a
      extract (CoState cont) = withOps envOps $
       do
        s <- ask
        cont s

stateDynComp4 :: forall m . (Monad m)
  => BaseComputation (EnvEff Int) (Return StateCompRes) m
stateDynComp4 = runPipeline
  statePipeline1 stateComp2

stateDynComp5 :: forall m . (Monad m)
  => BaseComputation Nil (Return StateCompRes) m
stateDynComp5 = bindOpsHandler
  (mkEnvHandler (6 :: Int))
  stateDynComp4

churchStateTest2 :: TestTree
churchStateTest2 = testCase "Church state test 2" $
  assertEqual "State ops pipeline should handle state correctly"
    (6, 8, 8) $
    runIdentityComp stateDynComp5

stateFreeComp1 :: forall m . (Monad m)
  => m (CoState Int m StateCompRes)
stateFreeComp1 = handleFree @FreeMonad stateCoOpHandler $
  withOps @(State Int) freeOps stateComp1

stateFreeComp2 :: Identity StateCompRes
stateFreeComp2 = stateDynComp2 >>= runCoState 7

freeStateTest1 :: TestTree
freeStateTest1 = testCase "Free state test 1" $
 assertEqual "State ops handler should handle state correctly"
  (7, 9, 9) $
  runIdentity stateFreeComp2
