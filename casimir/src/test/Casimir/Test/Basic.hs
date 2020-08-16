module Casimir.Test.Basic where

import Test.Tasty

import qualified Casimir.Test.Basic.Test1 as Test1

tests :: TestTree
tests = testGroup "Basic tests"
  [ Test1.tests
  ]
