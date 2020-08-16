module Casimir.Test.Main where

import Test.Tasty

import qualified Casimir.Test.Basic as Basic

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Casimir tests"
  [ Basic.tests
  ]
