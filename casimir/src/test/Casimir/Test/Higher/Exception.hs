module Casimir.Test.Higher.Exception
  ( exceptionTests
  )
where

import Test.Tasty hiding (withResource)
import Test.Tasty.HUnit

import Control.Monad.Identity

import Casimir.Base
import Casimir.Higher.Ops.Exception
import Casimir.Computation

import Casimir.Higher
  ( HigherPipeline
  , HigherComputation
  , coopHandlerToPipeline
  )

import Casimir.Higher.Monad.Church

import qualified Casimir.Base as Base
import qualified Casimir.Higher.Free as Higher

exceptionTests :: TestTree
exceptionTests = testGroup "ExceptionOps Tests"
  [ testException1
  , testException2
  ]

exceptionPipeline
  :: forall m e a
   . (Monad m)
  => HigherPipeline Nil (Multi '[ExceptionOps e]) (Return a) (Return (Either e a)) m m
exceptionPipeline = coopHandlerToPipeline @ChurchMonad $
  genericComputation exceptionCoOpHandler

-- data DivisionByZeroErr = DivisionByZeroErr

-- divide :: Int -> Int -> Eff (ExceptionOps DivisionByZeroErr) Int
-- divide a b =
--   if b == 0
--   then throw DivisionByZeroErr
--   else return $ truncate $
--     (fromIntegral a :: Double) / (fromIntegral b)

testException1 :: TestTree
testException1 =
  let
    comp1 :: Eff '[ExceptionOps String] Int
    comp1 = throw "error"

    comp2 :: Either String Int
    comp2 = runIdentity $
      Higher.withCoOpHandler @ChurchMonad exceptionCoOpHandler $
        comp1
  in
  testCase "Exception test 1" $ do
    assertEqual "trivial throw should return Left"
      (Left "error")
      comp2

testException2 :: TestTree
testException2 =
  let
    comp1 :: Eff '[ExceptionOps String] Int
    comp1 =
      tryCatch
        (throw "error")
        (\_ -> return 0)

    comp2 :: forall m . (Monad m)
      => HigherComputation Nil (Return (Either String Int)) m
    comp2 = runPipeline exceptionPipeline $
      genericReturn @(ExceptionOps String) comp1

    comp3 :: Either String Int
    comp3 = runIdentityComp comp2
  in
  testCase "Exception test 2" $ do
    assertEqual "try catch should recover error"
      (Right 0)
      comp3
