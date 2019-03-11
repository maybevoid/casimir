module Effect.Test.Ops where

import Test.Tasty

import Effect.Test.Ops.Env
import Effect.Test.Ops.State

opsTests :: TestTree
opsTests = testGroup "Ops Tests"
  [ envTests
  , stateTests
  ]