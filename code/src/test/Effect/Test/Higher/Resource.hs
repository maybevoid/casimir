module Effect.Test.Higher.Resource
  ( resourceTests
  )
where

import qualified Control.Effect.Implicit.Base as Base

import Test.Tasty hiding (withResource)
import Test.Tasty.HUnit

import Data.IORef
import Control.Exception
import Control.Monad.Trans.State.Strict (StateT, execStateT)

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher
import Control.Effect.Implicit.Ops.Io
import Control.Effect.Implicit.Ops.State
import Control.Effect.Implicit.Computation
import Control.Effect.Implicit.Transform.State
import Control.Effect.Implicit.Higher.Ops.Resource

resourceTests :: TestTree
resourceTests = testGroup "ResourceEff Tests"
  [ testResource1
  , testResource2
  ]

type BracketResourceEff = ResourceEff BracketResource

bracketHandler
  :: HigherOpsHandler NoEff BracketResourceEff IO
bracketHandler = baseOpsHandler ioBracketOps

ioHandler :: BaseOpsHandler NoEff IoEff IO
ioHandler = baseOpsHandler ioOps

stateTHandler
  :: forall s eff
   . (Effect eff)
  => BaseOpsHandler NoEff (StateEff s) (StateT s eff)
stateTHandler = baseOpsHandler stateTOps

pushRef :: forall a . IORef [a] -> a
     -> Eff IoEff ()
pushRef ref x = do
  xs <- liftIo $ readIORef ref
  liftIo $ writeIORef ref $ xs <> [x]

pushIo :: forall a . IORef [a] -> a -> IO ()
pushIo = Base.withOps ioOps pushRef

pushState
  :: forall a . a
  -> Eff (StateEff [a]) ()
pushState x = do
  xs <- get
  put $ xs <> [x]

makeResource
  :: String
  -> String
  -> IORef [String]
  -> BracketResource String
makeResource name value ref = BracketResource alloc release
  where
    alloc = do
      pushIo ref $ name <> ": alloc"
      return value

    release _ = do
      pushIo ref $ name <> ": release"
      return ()

comp1
  :: IORef [String]
  -> Eff (StateEff [String] ∪ IoEff ∪ BracketResourceEff) ()
comp1 ref = do
  push "outer-comp: start"
  res <- withResource resource1 $ \arg -> do
    push $ "inner-comp with argument: " <> arg
    return "inner-result"
  push $ "result from inner-comp: " <> res
 where
  push :: String -> Eff (StateEff [String] ∪ IoEff) ()
  push x = do
    pushRef ref x
    pushState x

  resource1 :: BracketResource String
  resource1 = makeResource "resource1" "foo" ref

pipeline1
  :: forall comp
   . (forall eff . (Effect eff)
      => BaseComputation
          (StateEff [String] ∪ IoEff ∪ BracketResourceEff)
          comp
          eff)
  -> HigherComputation NoEff comp (StateT [String] IO)
pipeline1 comp11 =
  bindOpsHandler (toHigherComputation stateTHandler) $
    liftComputation stateTHigherLiftEff $
      bindOpsHandler @(StateEff [String]) bracketHandler $
        toHigherComputation $
          bindOpsHandler @(StateEff [String] ∪ BracketResourceEff) ioHandler $
            comp11

comp2
  :: IORef [String]
  -> HigherComputation NoEff (Return ()) (StateT [String] IO)
comp2 ref = pipeline1 $ genericReturn $ comp1 ref

testResource1 :: TestTree
testResource1 = testCase "Resource test 2" $ do
  ref <- newIORef []
  s1 <- execStateT (execComp $ comp2 ref) []

  assertEqual "Happy path should update state correctly"
    [ "outer-comp: start"
    , "inner-comp with argument: foo"
    , "result from inner-comp: inner-result"
    ]
    s1

  s2 <- readIORef ref
  assertEqual "Happy path should update IORef correctly"
    [ "outer-comp: start"
    , "resource1: alloc"
    , "inner-comp with argument: foo"
    , "resource1: release"
    , "result from inner-comp: inner-result"
    ]
    s2

data DummyError = DummyError
  deriving (Show, Eq)

instance Exception DummyError

comp3
  :: IORef [String]
  -> Eff (StateEff [String] ∪ IoEff ∪ BracketResourceEff) ()
comp3 ref = do
  push "outer-comp: start"
  res1 <- withResource resource1 $ \arg1 -> do
    push $ "inner-comp-1 with argument: " <> arg1
    res2 <- withResource resource2 $ \arg2 -> do
      push $ "inner-comp-2 with argument: " <> arg2
      liftIo $ throwIO DummyError
    push $ "result from inner-comp-2: " <> res2
    return "inner-result-1"
  push $ "result from inner-comp: " <> res1
 where
  push :: String -> Eff (StateEff [String] ∪ IoEff) ()
  push x = do
    pushRef ref x
    pushState x

  resource1 :: BracketResource String
  resource1 = makeResource "resource1" "foo" ref

  resource2 :: BracketResource String
  resource2 = makeResource "resource2" "bar" ref

comp4
  :: IORef [String]
  -> HigherComputation NoEff (Return ()) (StateT [String] IO)
comp4 ref = pipeline1 $ genericReturn $ comp3 ref

testResource2 :: TestTree
testResource2 = testCase "Resource test 2" $ do
  ref <- newIORef []

  res <- try $ execStateT (execComp $ comp4 ref) []

  assertEqual "Computation should raise error"
    (Left DummyError) res

  s2 <- readIORef ref
  assertEqual "IORef should still log all alloc release"
    [ "outer-comp: start"
    , "resource1: alloc"
    , "inner-comp-1 with argument: foo"
    , "resource2: alloc"
    , "inner-comp-2 with argument: bar"
    , "resource2: release"
    , "resource1: release"
    ]
    s2
