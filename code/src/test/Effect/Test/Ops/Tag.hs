
module Effect.Test.Ops.Tag
where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Identity

import Control.Effect.Implicit
import Control.Effect.Implicit.Ops.Env
import Control.Effect.Implicit.Tagged.Ops.Env

taggedTests :: TestTree
taggedTests = testGroup "TaggedEff Tests"
  [ test1
  ]

data Foo
data Bar

instance ImplicitOps (TaggedEnvEff Foo e) where
  type OpsConstraint (TaggedEnvEff Foo e) eff
    = (?fooEnvOps :: TaggedEnvOps Foo e eff)

  withOps ops comp
    = let ?fooEnvOps = ops in comp

  captureOps = ?fooEnvOps

instance ImplicitOps (TaggedEnvEff Bar e) where
  type OpsConstraint (TaggedEnvEff Bar e) eff
    = (?barEnvOps :: TaggedEnvOps Bar e eff)

  withOps ops comp
    = let ?barEnvOps = ops in comp

  captureOps = ?barEnvOps

comp1
  :: Eff
      ( TaggedEnvEff Foo String
      ∪ TaggedEnvEff Bar String
      )
      String
comp1 = do
  fooVal <- askTag @Foo
  barVal <- askTag @Bar
  return $ fooVal ++ " " ++ barVal

fooOps
  :: forall eff . (Effect eff)
  => TaggedEnvOps Foo String eff
fooOps = mkTaggedEnvOps "foo"

barOps
  :: forall eff . (Effect eff)
  => TaggedEnvOps Bar String eff
barOps = mkTaggedEnvOps "bar"

fooBarOps
  :: forall eff . (Effect eff)
  => Operation
      ( (TaggedEnvEff Foo String)
      ∪ (EnvEff String)
      ∪ (TaggedEnvEff Bar String)
      )
      eff
fooBarOps = fooOps ∪ (mkEnvOps "default") ∪ barOps

res1 :: String
res1 = runIdentity $ withOps fooBarOps comp1

test1 :: TestTree
test1 = testCase "Tagged EnvEff test" $
  assertEqual
    "computation should be able to read correctly from two differently tagged env effs"
    "foo bar"
    res1

