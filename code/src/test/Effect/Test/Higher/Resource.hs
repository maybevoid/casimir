module Effect.Test.Higher.Resource where

import qualified Control.Effect.Implicit.Base as Base

import Test.Tasty hiding (withResource)
import Test.Tasty.HUnit

import Data.IORef
import Control.Monad.Trans.State.Strict (StateT, execStateT)

import Control.Effect.Implicit.Base
  (NoEff, (∪), type (∪))

import Control.Effect.Implicit.Higher
import Control.Effect.Implicit.Ops.Io
import Control.Effect.Implicit.Ops.State
import Control.Effect.Implicit.Transform.State
import Control.Effect.Implicit.Higher.Ops.Resource

resourceTests :: TestTree
resourceTests = testGroup "ResourceEff Tests"
  [ testResource1
  ]

pushRef :: forall a . IORef [a] -> a
     -> Eff IoEff NoEff ()
pushRef ref x = do
  xs <- liftIo $ readIORef ref
  liftIo $ writeIORef ref $ xs <> [x]

pushIo :: forall a . IORef [a] -> a -> IO ()
pushIo = Base.withOps ioOps pushRef

pushState
  :: forall a . a
  -> Eff (StateEff [a]) NoEff ()
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
  -> Eff (StateEff [String] ∪ IoEff) (ResourceEff BracketResource) ()
comp1 ref = do
  push "outer-comp: start"
  res <- withResource resource1 $ \arg -> do
    push $ "inner-comp with argument: " <> arg
    return "inner-result"
  push $ "result from inner-comp: " <> res

 where
  push :: String -> Eff (StateEff [String] ∪ IoEff) NoEff ()
  push x = do
    pushRef ref x
    pushState x

  resource1 :: BracketResource String
  resource1 = makeResource "resource1" "foo" ref

stateTBracketOps
  :: forall s
   . ResourceOps BracketResource (StateT s IO) (StateT s IO)
stateTBracketOps = invEffmap liftStateT stateTContraLift ioBracketOps

stateTIoOps
  :: forall s
   . IoOps (StateT s IO)
stateTIoOps = Base.effmap liftStateT ioOps

comp2 :: IORef [String] -> StateT [String] IO ()
comp2 ref =
  withOps
    (stateTIoOps ∪ stateTOps)
    stateTBracketOps $
      comp1 ref

testResource1 :: TestTree
testResource1 = testCase "Resource test 1" $ do
  ref <- newIORef []
  s1 <- execStateT (comp2 ref) []

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