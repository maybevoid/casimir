module Effect.Test.Ops.Env where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Effect.Implicit
import Control.Effect.Implicit.Ops.Env

envTests :: TestTree
envTests = testGroup "EnvEff Tests"
  [ envOpsTest
  , envHandlerTest
  , envPipelineTest
  ]

envComp1
  :: forall a . (Show a)
  => Eff (EnvEff a) String
envComp1 = do
  env <- ask
  return $ "Env: " ++ show env

envComp2 ::
  forall a .
  (Show a)
  => GenericReturn (EnvEff a) String
envComp2 = genericReturn envComp1

envOpsTest :: TestTree
envOpsTest = testCase "Env ops test" $
 do
  let envOps = mkEnvOps @Int @IO 3
  res <- withOps envOps envComp1
  assertEqual
    "Computation should read and format '3' from environment"
    res "Env: 3"

envHandlerTest :: TestTree
envHandlerTest = testCase "Env handler test" $
 do
  let
    envHandler = mkEnvHandler @Int @IO 4
    envComp3 =
      bindOpsHandlerWithCast @NoEff
        cast cast
        envHandler envComp2
  res <- execComp envComp3
  assertEqual
    "Computation should read and format '4' from environment"
    res "Env: 4"

envPipelineTest :: TestTree
envPipelineTest = testCase "Env pipeline test" $
 do
  let
    envHandler = mkEnvHandler @Int @IO 5
    envPipeline
      = opsHandlerToPipeline envHandler @(Return String)
    envComp3
      = runPipelineWithCast @NoEff
        cast cast
        envPipeline envComp2
  res <- execComp @NoEff envComp3
  assertEqual
    "Computation should read and format '5' from environment"
    res "Env: 5"
