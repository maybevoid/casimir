module Effect.Test.Higher.Exception
  ( exceptionTests
  )
where

import Test.Tasty hiding (withResource)
import Test.Tasty.HUnit

import Control.Monad.Identity

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.Ops.Exception
import Control.Effect.Implicit.Computation

import Control.Effect.Implicit.Higher
import Control.Effect.Implicit.Higher.Monad.Church

import qualified Control.Effect.Implicit.Higher.Free as Higher

exceptionTests :: TestTree
exceptionTests = testGroup "ExceptionEff Tests"
  [ testException1
  , testException2
  ]

exceptionPipeline
  :: forall eff e a
   . (Effect eff)
  => HigherPipeline NoEff (ExceptionEff e) (Return a) (Return (Either e a)) eff eff
exceptionPipeline = coopHandlerToPipeline @ChurchMonad $
  genericComputation exceptionCoOpHandler

-- data DivisionByZeroErr = DivisionByZeroErr

-- divide :: Int -> Int -> Eff (ExceptionEff DivisionByZeroErr) Int
-- divide a b =
--   if b == 0
--   then throw DivisionByZeroErr
--   else return $ truncate $
--     (fromIntegral a :: Double) / (fromIntegral b)

testException1 :: TestTree
testException1 =
  let
    comp1 :: Eff (ExceptionEff String) Int
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
    comp1 :: Eff (ExceptionEff String) Int
    comp1 =
      tryCatch
        (throw "error")
        (\_ -> return 0)

    comp2 :: forall eff . (Effect eff)
      => HigherComputation NoEff (Return (Either String Int)) eff
    comp2 = runPipeline exceptionPipeline $
      genericReturn @(ExceptionEff String) comp1

    comp3 :: Either String Int
    comp3 = runIdentityComp comp2
  in
  testCase "Exception test 2" $ do
    assertEqual "try catch should recover error"
      (Right 0)
      comp3
