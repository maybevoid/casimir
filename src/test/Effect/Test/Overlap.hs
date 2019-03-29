
module Effect.Test.Overlap
where

import Data.Proxy
import Control.Monad.Identity

import Test.Tasty
import Test.Tasty.HUnit

import Control.Effect.Implicit
import Control.Effect.Implicit.Ops
import Control.Effect.Implicit.TaggedOps.Env
import Control.Effect.Implicit.TaggedOps.TaggedEff

overlapTests :: TestTree
overlapTests = testGroup "Constraint overlap Tests"
  [ test1
  , test2
  , test3
  , test4
  , test5
  ]

data Foo
data Bar
data Baz

type FooEff = TaggedEnvEff Foo String
type BarEff = TaggedEnvEff Bar String
type BazEff = TaggedEnvEff Baz String

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

instance ImplicitOps (TaggedEnvEff Baz e) where
  type OpsConstraint (TaggedEnvEff Baz e) eff
    = (?bazEnvOps :: TaggedEnvOps Baz e eff)

  withOps ops comp
    = let ?bazEnvOps = ops in comp

  captureOps = ?bazEnvOps

fooOps1 :: Operation FooEff Identity
fooOps1 = mkTaggedEnvOps "foo1"

fooOps2 :: Operation FooEff Identity
fooOps2 = mkTaggedEnvOps "foo2"

barOps1 :: Operation BarEff Identity
barOps1 = mkTaggedEnvOps "bar1"

barOps2 :: Operation BarEff Identity
barOps2 = mkTaggedEnvOps "bar2"

bazOps :: Operation BazEff Identity
bazOps = mkTaggedEnvOps "baz"

-- Proxies are static tests to confirm the type family
-- transformations are as we expected

proxy1 :: forall ops1 ops2 . Proxy (Union ops1 ops2)
proxy1 = Proxy @ (ops1 ∪ ops2)

proxy2 :: forall ops1 ops2 ops3 . Proxy (Union ops1 (Union ops2 ops3))
proxy2 = Proxy @ (ops1 ∪ ops2 ∪ ops3)

proxy3 :: forall ops1 ops2 eff . Proxy (OpsConstraint (ops1 ∪ ops2) eff)
proxy3 = Proxy @ (OpsConstraint ops2 eff, OpsConstraint ops1 eff)

proxy4 :: forall ops1 ops2 ops3 eff . Proxy (OpsConstraint (ops1 ∪ ops2 ∪ ops3) eff)
proxy4 = Proxy @ ((OpsConstraint ops3 eff, OpsConstraint ops2 eff), OpsConstraint ops1 eff)

proxy5
  :: forall ops1 ops2 ops3 ops4 ops5 eff .
  Proxy (OpsConstraint ((ops1 ∪ ops2 ∪ ops3) ∪ (ops4 ∪ ops5)) eff)
proxy5 = Proxy @
  ( (OpsConstraint ops5 eff, OpsConstraint ops4 eff)
  , ((OpsConstraint ops3 eff, OpsConstraint ops2 eff), OpsConstraint ops1 eff)
  )

proxy6
  :: forall ops1 ops2 ops3 eff .
  Proxy (OpsConstraint ((ops1 ∪ ops2 ∪ ops3) ∪ (ops1 ∪ ops2)) eff)
proxy6 = Proxy @
  ( (OpsConstraint ops2 eff, OpsConstraint ops1 eff)
  , ((OpsConstraint ops3 eff, OpsConstraint ops2 eff), OpsConstraint ops1 eff)
  )

proxy7
  :: forall ops1 ops2 ops3 eff .
  Proxy (OpsConstraint ((ops1 ∪ ops2) ∪ (ops1 ∪ ops2 ∪ ops3)) eff)
proxy7 = Proxy @
  ( ((OpsConstraint ops3 eff, OpsConstraint ops2 eff), OpsConstraint ops1 eff)
  , (OpsConstraint ops2 eff, OpsConstraint ops1 eff)
  )

fooOps3 :: Operation (FooEff ∪ FooEff) Identity
fooOps3 = fooOps1 ∪ fooOps2

fooOps4 :: Operation (FooEff ∪ FooEff) Identity
fooOps4 = fooOps2 ∪ fooOps1

fooOps5 :: Operation FooEff Identity
fooOps5 = castOps cast fooOps3

fooOps6 :: Operation FooEff Identity
fooOps6 = castOps cast fooOps4

test1 :: TestTree
test1 = testCase "Binary ops cast" $ do
  assertEqual "cast should get second element (foo2)" "foo2" $
    askOp $ untagOps fooOps5

  assertEqual "cast should get second element (foo1)" "foo1" $
    askOp $ untagOps fooOps6

combinedOps1 :: Operation
  ( (FooEff ∪ BarEff)
  ∪ (FooEff ∪ BarEff ∪ BazEff)
  )
  Identity
combinedOps1 = (fooOps1 ∪ barOps1) ∪ (fooOps2 ∪ barOps2 ∪ bazOps)

combinedOps2 :: Operation
  ( (FooEff ∪ BarEff ∪ BazEff)
  ∪ (FooEff ∪ BarEff)
  )
  Identity
combinedOps2 = (fooOps2 ∪ barOps2 ∪ bazOps) ∪ (fooOps1 ∪ barOps1)

castedOps1 :: Operation (FooEff ∪ BarEff) Identity
castedOps1 = castOps cast combinedOps1

castedOps2 :: Operation (FooEff ∪ BarEff) Identity
castedOps2 = castOps cast combinedOps2

test2 :: TestTree
test2 = testCase "Nested ops cast" $ do
  assertEqual "cast should get second element (foo2)" "foo2" $
    askOp $ untagOps $ leftOps castedOps1

  assertEqual "cast should get second element (foo1)" "foo1" $
    askOp $ untagOps $ leftOps castedOps2

castedOps3 :: Operation (FooEff ∪ BarEff ∪ BazEff) Identity
castedOps3 = castOps cast combinedOps1

castedOps4 :: Operation (FooEff ∪ BarEff ∪ BazEff) Identity
castedOps4 = castOps cast combinedOps2

test3 :: TestTree
test3 = testCase "Nested ops cast 2" $ do
  assertEqual "cast should get second element (foo2)" "foo2" $
    askOp $ untagOps $ leftOps castedOps3

  assertEqual "cast should get second element (foo1)" "foo1" $
    askOp $ untagOps $ leftOps castedOps4

proxy8 :: Proxy (
  OpsConstraint
    ( (FooEff ∪ BarEff)
    ∪ (FooEff ∪ BarEff ∪ BazEff)
    )
    Identity
  )
proxy8 = Proxy @
  ( ( ( OpsConstraint BazEff Identity
      , OpsConstraint BarEff Identity)
    , OpsConstraint FooEff Identity)
  , ( OpsConstraint BarEff Identity
    , OpsConstraint FooEff Identity)
  )

proxy9 :: Proxy (
  OpsConstraint
    ( (FooEff ∪ BarEff ∪ BazEff)
    ∪ (FooEff ∪ BarEff)
    )
    Identity
  )
proxy9 = Proxy @
  ( ( OpsConstraint BarEff Identity
    , OpsConstraint FooEff Identity)
  , ( ( OpsConstraint BazEff Identity
      , OpsConstraint BarEff Identity)
    , OpsConstraint FooEff Identity)
  )

comp1 ::
  ( OpsConstraint BarEff Identity
  , OpsConstraint FooEff Identity
  )
  => String
comp1 = runIdentity $ askTag @Foo

test4 :: TestTree
test4 = testCase "Nested constraints" $ do
  assertEqual "ask should get second element (foo2)" "foo2" $
    withOps combinedOps1 comp1

  assertEqual "ask should get second element (foo1)" "foo1" $
    withOps combinedOps2 comp1

comp2 ::
  ( ( OpsConstraint BazEff Identity
    , OpsConstraint BarEff Identity
    )
  , OpsConstraint FooEff Identity
  )
  => [String]
comp2 = runIdentity $ do
  foo <- askTag @Foo
  bar <- askTag @Bar
  baz <- askTag @Baz
  return $ [foo, bar, baz]

test5 :: TestTree
test5 = testCase "Nested constraints 2" $ do
  assertEqual "ask should get second element (foo2, bar2, baz)" ["foo2", "bar2", "baz"] $
    withOps combinedOps1 comp2

  assertEqual "ask should get second element (foo1, bar1, baz)" ["foo1", "bar1", "baz"] $
    withOps combinedOps2 comp2
