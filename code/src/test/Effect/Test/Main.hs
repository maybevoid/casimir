module Effect.Test.Main (main) where

import Test.Tasty

import Effect.Test.Ops (opsTests)
import Effect.Test.Overlap (overlapTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Implicit Effects Tests"
  [ opsTests
  , overlapTests
  ]
