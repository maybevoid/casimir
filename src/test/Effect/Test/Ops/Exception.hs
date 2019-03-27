
module Effect.Test.Ops.Exception
where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Effect.Implicit

exceptionTests :: TestTree
exceptionTests = testGroup "Exception Tests"
  [ test1
  , test2
  , test3
  , test4
  ]

data Error = Error String
  deriving (Eq, Show)

divideComp1
  :: forall eff
   . (Effect eff, OpsConstraint (ExceptionEff Error) eff)
  => Int
  -> Int
  -> eff Int
divideComp1 x y =
  if y == 0
  then raise (Error "Division by zero")
  else return $ quot x y

testComp1
  :: forall eff . (Effect eff)
  => Int
  -> Int
  -> eff (Either Error Int)
testComp1 x y =
  withCoOpHandler @ChurchMonad exceptionToEitherHandler $
    divideComp1 x y

test1 :: TestTree
test1 = testCase
  "Divide by non zero should give right result" $
  do
    res <- testComp1 4 2
    assertEqual
      "Result should be (Right 2)"
      (Right 2)
      res

test2 :: TestTree
test2 = testCase
  "Divide by zero should give left error" $
  do
    res <- testComp1 1 0
    assertEqual
      "Result should be (Left err)"
      (Left (Error "Division by zero"))
      res

exceptionHandler
  :: forall eff . (Effect eff)
  => CoOpHandler (ExceptionEff Error) Int Int eff
exceptionHandler = mkExceptionCoOpHandler $
  \_ -> return 0

testComp2
  :: forall eff . (Effect eff)
  => Int
  -> Int
  -> eff Int
testComp2 x y =
  withCoOpHandler @ChurchMonad exceptionHandler $
    divideComp1 x y

test3 :: TestTree
test3 = testCase
  "Divide by non zero should give normal result" $
  do
    res <- testComp2 4 2
    assertEqual
      "Result should be 2"
      2
      res

test4 :: TestTree
test4 = testCase
  "Divide by zero should be handled" $
  do
    res <- testComp2 1 0
    assertEqual
      "Result should be 0"
      0
      res
