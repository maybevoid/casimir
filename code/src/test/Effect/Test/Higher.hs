module Effect.Test.Higher where

import Test.Tasty

import Effect.Test.Higher.Resource

higherOpsTests :: TestTree
higherOpsTests = testGroup "Higher Ops Tests"
  [ resourceTests
  ]
