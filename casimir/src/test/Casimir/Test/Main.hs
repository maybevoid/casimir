module Casimir.Test.Main (main) where

import Test.Tasty

import Casimir.Test.Ops (opsTests)
import Casimir.Test.Higher (higherOpsTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Casimir mects tests"
  [ opsTests
  , higherOpsTests
  ]
