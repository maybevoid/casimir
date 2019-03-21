
module Effect.Test.Ops.Tag
where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Identity

import Control.Effect

taggedTests :: TestTree
taggedTests = testGroup "TaggedEff Tests"
  [ test1
  ]

data Foo where
data Bar where

type FooEnvEff e = TaggedEff Foo (EnvEff e)
type FooEnvOps e eff = TaggedOps Foo (EnvOps e) eff
type FooEnvCoOp e r = TaggedCoOp Foo (EnvCoOp e) r
type FooEnvConstraint e eff = (?fooEnvOps :: FooEnvOps e eff)

instance EffOps (FooEnvEff e) where
  type OpsConstraint (FooEnvEff e) eff = FooEnvConstraint e eff

  withOps ops comp = let ?fooEnvOps = ops in comp
  captureOps = ?fooEnvOps

type BarEnvEff e = TaggedEff Bar (EnvEff e)
type BarEnvOps e eff = TaggedOps Bar (EnvOps e) eff
type BarEnvCoOp e r = TaggedCoOp Bar (EnvCoOp e) r
type BarEnvConstraint e eff = (?barEnvOps :: BarEnvOps e eff)

instance EffOps (BarEnvEff e) where
  type OpsConstraint (BarEnvEff e) eff = BarEnvConstraint e eff

  withOps ops comp = let ?barEnvOps = ops in comp
  captureOps = ?barEnvOps

askFoo
  :: forall eff e
   . ( Effect eff
     , FooEnvConstraint e eff
     )
  => eff e
askFoo = withTag @Foo @(EnvEff e) $ ask

askBar
  :: forall eff e
   . ( Effect eff
     , BarEnvConstraint e eff
     )
  => eff e
askBar = withTag @Bar @(EnvEff e) $ ask

comp1
  :: forall eff
   . ( Effect eff
     , FooEnvConstraint String eff
     , BarEnvConstraint String eff
     )
  => eff String
comp1 = do
  fooVal <- askFoo
  barVal <- askBar
  return $ fooVal ++ " " ++ barVal

fooOps
  :: forall eff . (Effect eff)
  => FooEnvOps String eff
fooOps = TaggedOps $ mkEnvOps "foo"

barOps
  :: forall eff . (Effect eff)
  => BarEnvOps String eff
barOps = TaggedOps $ mkEnvOps "bar"

fooBarOps
  :: forall eff . (Effect eff)
  => Operation (Union (FooEnvEff String) (BarEnvEff String)) eff
fooBarOps = UnionOps fooOps barOps

res1 :: String
res1 = runIdentity $ withOps fooBarOps comp1

test1 :: TestTree
test1 = testCase "Tagged EnvEff test" $
  assertEqual
    "computation should be able to read correctly from two differently tagged env effs"
    "foo bar"
    res1

