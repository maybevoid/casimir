module Casimir.Test.Basic.Test1 where

import Casimir.Base
import Casimir.Ops.Env
import Casimir.Ops.State
import Casimir.Ops.Env.MonadReader
import Casimir.Ops.State.MonadState

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans.State (StateT (..))
import Control.Monad.Trans.Reader (ReaderT (..))

tests :: TestTree
tests = testCase "Basic Test 1" $ do
  assertEqual "computation result should be foobar"
    "foobar"
    (runReaderState comp3)

  assertEqual "computation result should be foobar"
    "foobar"
    (runReaderState comp4)

  assertEqual "computation result should be foobar"
    "foobar"
    (runStateReader comp5)

  assertEqual "computation result should be foobar"
    "foobar"
    (runStateReader comp6)

comp1 :: Eff (Params '[EnvOps String, StateOps String]) String
comp1 = do
  s1 <- ask
  s2 <- get
  put $ s1 <> s2
  get

comp2 :: Eff (Params '[StateOps String, EnvOps String]) String
comp2 = comp1

ops1
  :: forall s e
   . Params '[StateOps s, EnvOps e] (ReaderT e (StateT s Identity))
ops1 = Cons stateOps $ Cons envOps Nil

ops2
  :: forall s e
   . Params '[EnvOps e, StateOps s] (ReaderT e (StateT s Identity))
ops2 = Cons envOps $ Cons stateOps Nil

ops3
  :: forall s e
   . Params '[StateOps s, EnvOps e] (StateT s (ReaderT e Identity))
ops3 = Cons stateOps $ Cons envOps Nil

ops4
  :: forall s e
   . Params '[EnvOps e, StateOps s] (StateT s (ReaderT e Identity))
ops4 = Cons envOps $ Cons stateOps Nil

comp3 :: ReaderT String (StateT String Identity) String
comp3 = withOps ops1 comp1

comp4 :: ReaderT String (StateT String Identity) String
comp4 = withOps ops2 comp2

comp5 :: StateT String (ReaderT String Identity) String
comp5 = withOps ops3 comp2

comp6 :: StateT String (ReaderT String Identity) String
comp6 = withOps ops4 comp1

runReaderState
  :: forall r
   . ReaderT String (StateT String Identity) r
  -> r
runReaderState m = fst $ runIdentity $
  runStateT (runReaderT m "foo") "bar"

runStateReader
  :: forall r
   . StateT String (ReaderT String Identity) r
  -> r
runStateReader m = fst $ runIdentity $
  runReaderT (runStateT m "bar") "foo"
