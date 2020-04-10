module Casimir.Test.Ops where

import Test.Tasty

import Casimir.Test.Ops.Env
import Casimir.Test.Ops.State
import Casimir.Test.Ops.Pipe
import Casimir.Test.Ops.Exception
import Casimir.Test.Ops.Ambivalent

opsTests :: TestTree
opsTests = testGroup "Ops Tests"
  [ envTests
  , stateTests
  , pipeTests
  , exceptionTests
  , ambivalentTests
  ]
