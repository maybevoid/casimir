{-# LANGUAGE DeriveFunctor #-}

module Effect.Test.Ops.Pipe where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Trans.Free
import Control.Monad.Trans.Class

import Control.Effect

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

instance FreeOps (YieldEff a) where
  type Operation (YieldEff a) = YieldOps a
  type CoOperation (YieldEff a) = YieldCoOps a

  freeOps liftCoOps = YieldOps $
    \x -> liftF $ liftCoOps $ YieldOp x id

instance FreeOps (AwaitEff a) where
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

runPipe :: forall a r ops eff1
   . (Effect eff1, EffOps ops)
  => Computation (Union (YieldEff a) ops) (Return r) eff1
  -> Computation (Union (AwaitEff a) ops) (Return r) eff1
  -> Computation ops (Return r) eff1
runPipe producer1 consumer1 = Computation comp
   where
    comp :: forall eff2 . (Effect eff2)
      => LiftEff eff1 eff2
      -> Operation ops eff2
      -> Return r eff2
    comp liftEff ops = Return $ pipe producer2 consumer2
     where
      producer2 :: FreeT (YieldCoOps a) eff2 r
      producer2 = returnVal $ runComp producer1
        (lift . liftEff) $
        UnionOps (freeOps id) (effmap lift ops)

      consumer2 :: FreeT (AwaitCoOps a) eff2 r
      consumer2 = returnVal $ runComp consumer1
        (lift . liftEff) $
        UnionOps (freeOps id) (effmap lift ops)

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

producerComp
  :: forall a .
  GenericReturn (Union (YieldEff Int) (EnvEff Int)) a
producerComp = genericReturn comp1
 where
  comp1 :: forall eff
    . ( Effect eff
      , YieldConstraint Int eff
      , EnvConstraint Int eff
      )
   => eff a
  comp1
   = do
      seed <- ask
      comp2 seed
       where
        comp2 acc = do
          yield acc
          comp2 $ acc + 1

consumerComp
  :: GenericReturn (Union (AwaitEff Int) (EnvEff Int)) Int
consumerComp = genericReturn $
 do
  x <- await
  y <- await
  z <- await
  return $ x + y + z

pipedComp :: forall eff . (Effect eff)
  => Computation (EnvEff Int) (Return Int) eff
pipedComp = runPipe producerComp consumerComp

pipeTest :: TestTree
pipeTest = testCase
  "Mutually recursive pipe computation should run correctly" $
  do
    res <- returnVal $ runComp pipedComp id (mkEnvOps 5)
    assertEqual
      "Pipe computation should return 18"
      18 res

pipeTests :: TestTree
pipeTests = testGroup "PipeEff Tests"
  [
    pipeTest
  ]