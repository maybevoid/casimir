
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

stateComp2 :: GenericComputation (StateEff Int) StateCompRes
stateComp2 = genericComputation stateComp1

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

stateDynComp1
  :: forall eff .
  (Effect eff)
  => DynamicEff (StateEff Int) eff StateCompRes
stateDynComp1 = bindConstraint @(StateEff Int) dynamicOps stateComp1

type CoState s eff a = (s, s -> eff a)

stateOpsHandler
  :: forall eff s a .
  (Effect eff)
  => s
  -> OpsHandler (StateEff s) a (CoState s eff a) eff
stateOpsHandler i = OpsHandler {
  handleReturn = handleReturn',
  handleOps = handleOps'
}
 where
  handleReturn' :: a -> eff (CoState s eff a)
  handleReturn' x = (return $ (i, \_ -> return x))

  handleOps' :: StateModel s (eff (CoState s eff a)) -> eff (CoState s eff a)
  handleOps' (GetOp cont1) = return $
    ( i
    , \s ->
      do
        (_, cont2) <- cont1 s
        cont2 s
    )
  handleOps' (PutOp s cont1) = return $
    ( i
    , \_ ->
      do
        (_, cont2) <- cont1 ()
        cont2 s
    )

stateDynComp2 :: forall eff . (Effect eff)
  => eff (CoState Int eff StateCompRes)
stateDynComp2 = withOpsHandler (stateOpsHandler 5) stateDynComp1

stateDynComp3 :: Identity StateCompRes
stateDynComp3 =
 do
  (i, cont) <- stateDynComp2
  cont i

dynStateTest1 :: TestTree
dynStateTest1 = testCase "Dynamic state test 1" $
 assertEqual "State ops handler should handle state correctly"
  (5, 7, 7) $
  runIdentity stateDynComp3

stateTests :: TestTree
stateTests = testGroup "StateEff Tests"
  [ stateTHandlerTest
  , ioStateTest
  , dynStateTest1
  ]