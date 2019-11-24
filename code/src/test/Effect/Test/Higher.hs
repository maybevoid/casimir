module Effect.Test.Higher where

import Test.Tasty

import Effect.Test.Higher.Resource
import Effect.Test.Higher.Exception

higherOpsTests :: TestTree
higherOpsTests = testGroup "Higher Ops Tests"
  [ resourceTests
  , exceptionTests
  ]
