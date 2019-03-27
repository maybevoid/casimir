module Effect.Test.Main (main) where

import Test.Tasty

import Effect.Test.Ops (opsTests)
import Effect.Test.Cast (castTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Implicit Effects Tests"
  [ opsTests
  , castTests
  ]
