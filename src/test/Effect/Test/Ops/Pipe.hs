{-# LANGUAGE DeriveFunctor #-}

module Effect.Test.Ops.Pipe where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Identity
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class

import Control.Effect
  hiding (Pure)

data YieldEff a where
data AwaitEff a where

data YieldOps a eff = YieldOps {
  yieldOp :: a -> eff ()
}

data AwaitOps a eff = AwaitOps {
  awaitOp :: eff a
}

data YieldCoOps a r =
  YieldOp a (() -> r)
  deriving (Functor)

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

  freeOps liftCoOps = YieldOps $
    \x -> liftF $ liftCoOps $ YieldOp x id

instance FreeEff (AwaitEff a) where
  type Operation (AwaitEff a) = AwaitOps a
  type CoOperation (AwaitEff a) = AwaitCoOps a

  freeOps liftCoOps = AwaitOps $
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

runPipe :: forall a r eff
   . (Effect eff)
  => Computation (YieldEff a) (Return r) eff
  -> Computation (AwaitEff a) (Return r) eff
  -> eff r
runPipe producer1 consumer1
  = pipe producer2 consumer2
   where
    producer2 :: FreeT (YieldCoOps a) eff r
    producer2 = returnVal $ runComp producer1 lift (freeOps id)

    consumer2 :: FreeT (AwaitCoOps a) eff r
    consumer2 = returnVal $ runComp consumer1 lift (freeOps id)

pipe
  :: forall a r eff
   . (Effect eff)
  => FreeT (YieldCoOps a) eff r
  -> FreeT (AwaitCoOps a) eff r
  -> eff r
pipe producer consumer = runFreeT consumer >>= handleConsumer
 where
  handleConsumer
    :: FreeF (AwaitCoOps a) r (FreeT (AwaitCoOps a) eff r)
    -> eff r
  handleConsumer (Pure r) = return r
  handleConsumer
    (Free (AwaitOp
      (cont :: a -> (FreeT (AwaitCoOps a) eff r))))
    = copipe cont producer

copipe
  :: forall a r eff
   . (Effect eff)
  => (a -> FreeT (AwaitCoOps a) eff r)
  -> FreeT (YieldCoOps a) eff r
  -> eff r
copipe consumer producer = runFreeT producer >>= handleProducer
 where
  handleProducer
    :: FreeF (YieldCoOps a) r (FreeT (YieldCoOps a) eff r)
    -> eff r
  handleProducer (Pure r) = return r
  handleProducer
    (Free (YieldOp x
      (cont :: () -> (FreeT (YieldCoOps a) eff r))))
    = pipe (cont ()) $ consumer x

producerComp :: forall a . GenericReturn (YieldEff Int) a
producerComp = genericReturn comp
 where
  comp :: forall eff
   . (Effect eff, YieldConstraint Int eff)
   => eff a
  comp
   = do
      yield 1
      yield 2
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
pipedComp = runPipe producerComp consumerComp

pipeTest :: TestTree
pipeTest = testCase
  "Mutually recursive pipe computation should run correctly" $
  assertEqual
    "Pipe computation should return 4"
    4 $
    runIdentity pipedComp

pipeTests :: TestTree
pipeTests = testGroup "PipeEff Tests"
  [
    pipeTest
  ]