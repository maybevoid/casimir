module Effect.Test.Ops.Env where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Effect

mkEnvOps :: forall a eff . (Effect eff) => a -> EnvOps a eff
mkEnvOps x = EnvOps {
  askOp = return x
}

envComp1
  :: forall eff a .
  (Effect eff, OpsConstraint (EnvEff a) eff, Show a)
  => eff String
envComp1 = do
  env <- ask
  return $ "Env: " ++ (show env)

envComp2 ::
  forall a .
  (Show a)
  => GenericComputation (EnvEff a) String
envComp2 = genericComputation envComp1

envOpsTest :: TestTree
envOpsTest = testCase "Env ops test" $
 do
  let envOps = mkEnvOps @Int @IO 3
  res <- bindConstraint envOps envComp1
  assertEqual
    "Computation should read and format '3' from environment"
    res "Env: 3"

envHandlerTest :: TestTree
envHandlerTest = testCase "Env handler test" $
 do
  let
    envHandler = mkEnvHandler @Int @IO 4
    envComp3 =
      bindHandlerWithCast @NoEff
        envHandler envComp2
        cast cast
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
      = handlerToPipeline envHandler @(Return String)
    envComp3
      = runPipelineWithCast @NoEff
        envPipeline envComp2 cast cast
  res <- execComp @NoEff envComp3
  assertEqual
    "Computation should read and format '5' from environment"
    res "Env: 5"

envTests :: TestTree
envTests = testGroup "EnvEff Tests"
  [ envOpsTest
  , envHandlerTest
  , envPipelineTest
  ]