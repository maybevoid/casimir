
module Effect.Test.Ops.Resource where

import Test.Tasty
import Test.Tasty.HUnit

import Data.IORef
import Control.Monad.Identity

import Control.Effect.Implicit
import Control.Effect.Implicit.Ops.Io
import Control.Effect.Implicit.Ops.Env
import Control.Effect.Implicit.Ops.State
import Control.Effect.Implicit.Ops.UnliftIo
import Control.Effect.Implicit.Ops.Resource
import Control.Effect.Implicit.Transform.State

import Control.Monad.Trans.State.Strict (StateT, runStateT)

resourceTests :: TestTree
resourceTests = testGroup "ResourceEff Tests"
  [ testUnliftIo1
  , testUnliftIo2
  , testResource1
  ]

unliftComp1
  :: forall eff1 eff2
   . ( Effect eff1
     , EffConstraint (FixedUnliftIoEff IoEff Identity eff1 ∪ IoEff) eff2
     )
  => eff2 Int
unliftComp1 = do
  ref <- liftIo$ newIORef (1 :: Int)
  fixedRunInIo $ do
    liftIo $ writeIORef ref 2
    fixedRunInIo $ do
      liftIo $ writeIORef ref 3
      fixedRunInIo $ do
        liftIo $ writeIORef ref 4
  liftIo $ readIORef ref

testUnliftIo1 :: TestTree
testUnliftIo1 = testCase "UnliftIo test 1" $ do
  res <- withOps (ioUnliftIoOps ∪ ioOps) unliftComp1
  assertEqual "Computation should update IORef to 4"
    4 res

stateCompTransform
  :: forall ops eff s a
   . ( Effect eff
     , ImplicitOps ops
     )
  => s
  -> Computation (StateEff s ∪ ops) (Return a) eff
  -> Computation ops (Return (s, a)) eff
stateCompTransform s comp1 = Computation comp2
 where
  comp2 :: forall eff2 . (Effect eff2)
    => LiftEff eff eff2
    -> Operation ops eff2
    -> Return (s, a) eff2
  comp2 lift ops = Return $ do
    (a, s') <- runStateT comp3 s
    return (s', a)
   where
    comp3 :: StateT s eff2 a
    comp3 = returnVal $ runComp comp1
      (joinLift lift stateTLiftEff)
      (stateTOps ∪ applyEffmap stateTLiftEff ops)

envCompTransform
  :: forall ops eff e a
   . ( Effect eff
     , ImplicitOps ops
     )
  => e
  -> Computation (EnvEff e ∪ ops) (Return a) eff
  -> Computation ops (Return a) eff
envCompTransform e comp1 = bindOps (mkEnvOps e) comp1

stateEnvIoUnlift
  :: forall s e a
   . e
  -> s
  -> Computation
      (IoEff ∪ StateEff s ∪ EnvEff e)
      (Return a)
      IO
  -> IO (s, a)
stateEnvIoUnlift e s comp1 = returnVal $
  runComp comp2 idLift NoOp
 where
  comp2 =
    bindOps ioOps $
    envCompTransform e $
    stateCompTransform s $
    castComputation cast comp1

stateEnvIoUnlift2
  :: forall s e a eff
   . (EffConstraint (StateEff s ∪ EnvEff e) eff)
  => eff (Computation
            (IoEff ∪ StateEff s ∪ EnvEff e)
            (Return a)
            IO
          -> IO (s, a))
stateEnvIoUnlift2 = do
  e <- ask
  s <- get
  return $ stateEnvIoUnlift e s

extractStateRes
  :: forall s a eff
   . (EffConstraint (StateEff s) eff)
  => (s, a)
  -> eff a
extractStateRes (s, x) = do
  put s
  return x

unliftOps1
  :: forall s e eff
   . (EffConstraint (StateEff s ∪ EnvEff e) eff)
  => UnliftIoOps (IoEff ∪ StateEff s ∪ EnvEff e) ((,) s) IO eff
unliftOps1 = mkUnliftIoOps stateEnvIoUnlift2 extractStateRes

unliftOps2
  :: forall s e eff
   . (EffConstraint (StateEff s ∪ EnvEff e) eff)
  => FixedUnliftIoOps (IoEff ∪ StateEff s ∪ EnvEff e) ((,) s) IO eff
unliftOps2 = mkFixedUnliftIoOps' @IoEff @(StateEff s ∪ EnvEff e) unliftOps1

inc :: forall eff
   . (EffConstraint (StateEff Int ∪ EnvEff Int) eff)
  => Int
  -> eff ()
inc x = do
  w <- ask
  s <- get
  put $ s + (x * w)

unliftComp2
  :: forall eff1 eff2 res
   . ( Effect eff1
     , EffConstraint
        ( FixedUnliftIoEff
            (IoEff ∪ StateEff Int ∪ EnvEff Int)
            res
            eff1
        ∪ IoEff ∪ StateEff Int ∪ EnvEff Int
        )
        eff2
     )
  => eff2 Int
unliftComp2 = do
  inc 1
  ref <- liftIo$ newIORef (1 :: Int)
  fixedRunInIo $ do
    inc 2
    fixedRunInIo $ do
      inc 3
      fixedRunInIo $ do
        inc 4
        liftIo $ writeIORef ref 4
  x <- get
  y <- liftIo $ readIORef ref
  return $ x + y

unliftComp3
  :: forall eff
   . (EffConstraint
       (IoEff ∪ StateEff Int ∪ EnvEff Int)
        eff)
  => eff Int
unliftComp3 = withOps unliftOps2 unliftComp2

unliftComp4 :: IO Int
unliftComp4 =
  withOps ioOps $
  withEnv 3 $
  withStateTAndOps @(EnvEff Int ∪ IoEff) 5 $
    unliftComp3

testUnliftIo2 :: TestTree
testUnliftIo2 = testCase "UnliftIo test 2" $ do
  res <- unliftComp4
  assertEqual "Computation should return 39"
    39 res

resourceOps1
  :: ResourceOps
    BracketTask
    (FixedUnliftIoEff IoEff Identity IO ∪ IoEff)
    IO IO
resourceOps1 = withOps (ioUnliftIoOps ∪ ioOps) $ bracketResourceOps'

resourceOps2
  :: FixedResourceOps
    BracketTask
    (FixedUnliftIoEff IoEff Identity IO ∪ IoEff)
    IO IO
resourceOps2 = mkFixedResourceOps resourceOps1

dummyBracketTask :: forall a . a -> BracketTask a
dummyBracketTask x = BracketTask (return x) $ \_ -> return ()

resourceComp1
  :: forall eff1 eff2 ops
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint
        (FixedResourceEff
          BracketTask
          (ops ∪ IoEff)
         eff1
        )
        eff2
     )
  => eff2 Int
resourceComp1 = do
  withFixedResource (dummyBracketTask (1 :: Int)) $ \x1 ->
    withFixedResource (dummyBracketTask (x1 + 2)) $ \x2 ->
      withFixedResource (dummyBracketTask (x2 + 3)) $ \x3 ->
        return $ x3 + 4

resourceComp2 :: IO Int
resourceComp2 = withOps resourceOps2 resourceComp1

testResource1 :: TestTree
testResource1 = testCase "Resource test 1" $ do
  res <- resourceComp2
  assertEqual "Computation should return 10"
    10 res
