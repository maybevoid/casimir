{-# LANGUAGE DeriveFunctor #-}

module Effect.Test.Ops.Pipe where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Identity
import Control.Monad.Trans.Free (liftF)

import Control.Effect

data YieldEff a where
data AwaitEff a where

data YieldOps a eff = YieldOps {
  yieldOp :: a -> eff ()
}

data YieldCoOps a r =
  YieldOp a (() -> r)
  deriving (Functor)

data AwaitOps a eff = AwaitOps {
  awaitOp :: eff a
}

data AwaitCoOps a r =
  AwaitOp (a -> r)
  deriving (Functor)

type YieldConstraint a eff = (?yieldOps :: YieldOps a eff)
type AwaitConstraint a eff = (?awaitOps :: AwaitOps a eff)

instance EffFunctor (YieldOps a) where
  effmap liftEff ops = YieldOps $
    \x -> liftEff $ yieldOp ops x

instance EffFunctor (AwaitOps a) where
  effmap liftEff ops = AwaitOps $
    liftEff $ awaitOp ops

instance FreeEff (YieldEff a) where
  type Operation (YieldEff a) = YieldOps a
  type CoOperation (YieldEff a) = YieldCoOps a

  freeMonad liftCoOps = YieldOps $
    \x -> liftF $ liftCoOps $ YieldOp x id

instance FreeEff (AwaitEff a) where
  type Operation (AwaitEff a) = AwaitOps a
  type CoOperation (AwaitEff a) = AwaitCoOps a

  freeMonad liftCoOps = AwaitOps $
    liftF $ liftCoOps $ AwaitOp id

instance EffOps (YieldEff a) where
  type OpsConstraint (YieldEff a) eff = YieldConstraint a eff

  bindConstraint yieldOps comp
    = let ?yieldOps = yieldOps in comp

  captureOps = ?yieldOps

instance EffOps (AwaitEff a) where
  type OpsConstraint (AwaitEff a) eff = AwaitConstraint a eff

  bindConstraint awaitOps comp
    = let ?awaitOps = awaitOps in comp

  captureOps = ?awaitOps

instance DynamicOps (YieldEff a) where
  dynamicOps = YieldOps $
    \x -> liftOps $ YieldOp x return

instance DynamicOps (AwaitEff a) where
  dynamicOps = AwaitOps $
    liftOps $ AwaitOp return

yield :: forall a eff
   . (Effect eff, YieldConstraint a eff)
  => a
  -> eff ()
yield = yieldOp ?yieldOps

await :: forall a eff
   . (Effect eff, AwaitConstraint a eff)
  => eff a
await = awaitOp ?awaitOps

pipe :: forall a r eff
   . (Effect eff)
  => Computation (YieldEff a) (Return r) eff
  -> Computation (AwaitEff a) (Return r) eff
  -> eff r
pipe producer consumer = withOpsHandler handler1 $
  returnVal $ runComp consumer liftDynamicEff captureOps
 where
  handler1 :: OpsHandler (AwaitEff a) r r eff
  handler1 = OpsHandler return handleAwait

  handleAwait :: AwaitCoOps a (eff r) -> eff r
  handleAwait (AwaitOp cont1) = copipe cont2 producer
   where
    cont2 :: a -> Computation (AwaitEff a) (Return r) eff
    cont2 x = Computation $ \liftEff _ ->
      Return $ liftEff $ cont1 x

copipe :: forall a r eff
   . (Effect eff)
  => (a -> Computation (AwaitEff a) (Return r) eff)
  -> Computation (YieldEff a) (Return r) eff
  -> eff r
copipe consumer producer = withOpsHandler handler1 $
  returnVal $ runComp producer liftDynamicEff captureOps
 where
  handler1 :: OpsHandler (YieldEff a) r r eff
  handler1 = OpsHandler return handleYield

  handleYield :: YieldCoOps a (eff r) -> eff r
  handleYield (YieldOp x cont1) = pipe cont2 $ consumer x
   where
    cont2 :: Computation (YieldEff a) (Return r) eff
    cont2 = Computation $ \liftEff _ ->
      Return $ liftEff $ cont1 ()

producerComp :: forall a . GenericReturn (YieldEff Int) a
producerComp = genericReturn comp
 where
  comp :: forall eff
   . (Effect eff, YieldConstraint Int eff)
   => eff a
  comp
   = do
      yield 1
      comp

consumerComp :: GenericReturn (AwaitEff Int) Int
consumerComp = genericReturn $
 do
  x <- await
  y <- await
  z <- await
  return $ x + y + z

pipedComp :: forall eff . (Effect eff)
  => eff Int
pipedComp = pipe producerComp consumerComp

pipeTest :: TestTree
pipeTest = testCase
  "Mutually recursive pipe computation should run correctly" $
  assertEqual
    "Pipe computation should return 3"
    3 $
    runIdentity pipedComp

pipeTests :: TestTree
pipeTests = testGroup "PipeEff Tests"
  [ pipeTest
  ]