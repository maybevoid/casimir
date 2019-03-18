module Effect.Test.Ops where

import Test.Tasty

import Effect.Test.Ops.Env
import Effect.Test.Ops.State
import Effect.Test.Ops.Pipe
import Effect.Test.Ops.Exception

opsTests :: TestTree
opsTests = testGroup "Ops Tests"
  [ envTests
  , stateTests
  , pipeTests
  , exceptionTests
  ]