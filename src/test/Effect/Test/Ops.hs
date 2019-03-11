module Effect.Test.Ops where

import Test.Tasty

import Effect.Test.Ops.Env

opsTests :: TestTree
opsTests = testGroup "Ops Tests"
  [ envTests
  ]