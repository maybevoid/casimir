module Casimir.Test.Higher where

import Test.Tasty

import Casimir.Test.Higher.Resource
import Casimir.Test.Higher.Exception

higherOpsTests :: TestTree
higherOpsTests = testGroup "Higher Ops Tests"
  [ resourceTests
  , exceptionTests
  ]
