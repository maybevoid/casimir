module Effect.Test.Ops.Env where

import Test.Tasty
import Test.Tasty.HUnit

import Casimir
import Casimir.Ops.Env

envTests :: TestTree
envTests = testGroup "EnvOps Tests"
  [ envOpsTest
  , envHandlerTest
  , envPipelineTest
  ]

envComp1
  :: forall a
   . (Show a)
  => Eff (EnvEff a) String
envComp1 = do
  env <- ask
  return $ "Env: " ++ show env

envComp2 ::
  forall a eff .
  (Show a, Effect eff)
  => BaseComputation (EnvEff a) (Return String) eff
envComp2 = genericReturn envComp1

envOpsTest :: TestTree
envOpsTest = testCase "Env ops test" $
 do
  let envOps = mkEnvOps @Int @IO 3
  res <- withOps envOps envComp1
  assertEqual
    "Computation should read and format '3' from environment"
    "Env: 3"
    res

envHandlerTest :: TestTree
envHandlerTest = testCase "Env handler test" $
 do
  let
    envHandler = mkEnvHandler @Int @IO 4
    envComp3 =
      bindOpsHandler @NoEff
        envHandler envComp2
  res <- execComp envComp3
  assertEqual
    "Computation should read and format '4' from environment"
    "Env: 4"
    res

envPipelineTest :: TestTree
envPipelineTest = testCase "Env pipeline test" $
 do
  let
    envHandler = mkEnvHandler @Int @IO 5
    envPipeline
      = opsHandlerToPipeline envHandler @(Return String)
    envComp3
      = runPipeline @NoEff
        envPipeline envComp2
  res <- execComp @NoEff envComp3
  assertEqual
    "Computation should read and format '5' from environment"
    "Env: 5"
    res
