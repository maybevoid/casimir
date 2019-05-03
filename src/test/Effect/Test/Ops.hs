module Effect.Test.Ops where

import Test.Tasty

import Effect.Test.Ops.Env
import Effect.Test.Ops.State
import Effect.Test.Ops.Pipe
import Effect.Test.Ops.Exception
import Effect.Test.Ops.Ambivalent
import Effect.Test.Ops.Tag
import Effect.Test.Ops.Resource

opsTests :: TestTree
opsTests = testGroup "Ops Tests"
  [ envTests
  , stateTests
  , pipeTests
  , exceptionTests
  , ambivalentTests
  , taggedTests
  , resourceTests
  ]