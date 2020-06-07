{-# LANGUAGE DeriveFunctor #-}

module Casimir.Test.Ops.Pipe where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Trans.Free
import Control.Monad.Trans.Class

import Casimir
import Casimir.Free
import Casimir.Ops.Env

data YieldEff a
data AwaitEff a

data YieldOps a eff = YieldOps {
  yieldOp :: a -> eff ()
}

data AwaitOps a eff = AwaitOps {
  awaitOp :: eff a
}

data YieldCoOp a r =
  YieldOp a (() -> r)
  deriving (Functor)

data AwaitCoOp a r =
  AwaitOp (a -> r)
  deriving (Functor)

instance EffOps (YieldEff a) where
  type Operation (YieldEff a) = YieldOps a

instance EffOps (AwaitEff a) where
  type Operation (AwaitEff a) = AwaitOps a

instance EffCoOp (YieldEff a) where
  type CoOperation (YieldEff a) = YieldCoOp a

instance EffCoOp (AwaitEff a) where
  type CoOperation (AwaitEff a) = AwaitCoOp a

instance EffFunctor Lift (YieldOps a) where
  effmap (Lift lift2) ops = YieldOps $
    \x -> lift2 $ yieldOp ops x

instance EffFunctor Lift (AwaitOps a) where
  effmap (Lift lift2) ops = AwaitOps $
    lift2 $ awaitOp ops

instance FreeOps (YieldEff a) where
  mkFreeOps liftCoOp = YieldOps $
    \x -> liftCoOp $ YieldOp x id

instance FreeOps (AwaitEff a) where
  mkFreeOps liftCoOp = AwaitOps $
    liftCoOp $ AwaitOp id

instance ImplicitOps (YieldEff a) where
  type OpsConstraint (YieldEff a) eff =
    (?yieldOps :: YieldOps a eff)

  withOps yieldOps comp
    = let ?yieldOps = yieldOps in comp

  captureOps = ?yieldOps

instance ImplicitOps (AwaitEff a) where
  type OpsConstraint (AwaitEff a) eff =
    (?awaitOps :: AwaitOps a eff)

  withOps awaitOps comp
    = let ?awaitOps = awaitOps in comp

  captureOps = ?awaitOps

yield :: forall a . a -> Eff (YieldEff a) ()
yield = yieldOp ?yieldOps

await :: forall a . Eff (AwaitEff a) a
await = awaitOp ?awaitOps

runPipe :: forall a r ops eff1
   . ( Monad eff1
   , EffOps ops
   , EffFunctor Lift (Operation ops)
   )
  => BaseComputation ((YieldEff a) ∪ ops) (Return r) eff1
  -> BaseComputation ((AwaitEff a) ∪ ops) (Return r) eff1
  -> BaseComputation ops (Return r) eff1
runPipe producer1 consumer1 = Computation comp
   where
    comp :: forall eff2 . (Monad eff2)
      => Lift eff1 eff2
      -> Operation ops eff2
      -> Return r eff2
    comp lifter ops = Return $ pipe producer2 consumer2
     where
      producer2 :: FreeT (YieldCoOp a) eff2 r
      producer2 = returnVal $ runComp producer1
        (joinLift lifter (Lift lift)) $
        (mkFreeOps liftF) ∪ (effmap (Lift lift) ops)

      consumer2 :: FreeT (AwaitCoOp a) eff2 r
      consumer2 = returnVal $ runComp consumer1
        (joinLift lifter (Lift lift)) $
        (mkFreeOps liftF) ∪ (effmap (Lift lift) ops)

pipe
  :: forall a r eff
   . (Monad eff)
  => FreeT (YieldCoOp a) eff r
  -> FreeT (AwaitCoOp a) eff r
  -> eff r
pipe producer consumer = runFreeT consumer >>= handleConsumer
 where
  handleConsumer
    :: FreeF (AwaitCoOp a) r (FreeT (AwaitCoOp a) eff r)
    -> eff r
  handleConsumer (Pure r) = return r
  handleConsumer
    (Free (AwaitOp
      (cont :: a -> (FreeT (AwaitCoOp a) eff r))))
    = copipe cont producer

copipe
  :: forall a r eff
   . (Monad eff)
  => (a -> FreeT (AwaitCoOp a) eff r)
  -> FreeT (YieldCoOp a) eff r
  -> eff r
copipe consumer producer = runFreeT producer >>= handleProducer
 where
  handleProducer
    :: FreeF (YieldCoOp a) r (FreeT (YieldCoOp a) eff r)
    -> eff r
  handleProducer (Pure r) = return r
  handleProducer
    (Free (YieldOp x
      (cont :: () -> (FreeT (YieldCoOp a) eff r))))
    = pipe (cont ()) $ consumer x

producerComp
  :: forall a eff
   . (Monad eff)
  => BaseComputation ((YieldEff Int) ∪ (EnvEff Int)) (Return a) eff
producerComp = genericReturn comp1
 where
  comp1 :: Eff (EnvEff Int ∪ YieldEff Int) a
  comp1
   = do
      seed <- ask
      comp2 seed
       where
        comp2 acc = do
          yield acc
          comp2 $ acc + 1

consumerComp
  :: forall eff . (Monad eff)
  => BaseComputation ((AwaitEff Int) ∪ (EnvEff Int)) (Return Int) eff
consumerComp = genericReturn $
 do
  x <- await
  y <- await
  z <- await
  return $ x + y + z

pipedComp :: forall eff . (Monad eff)
  => BaseComputation (EnvEff Int) (Return Int) eff
pipedComp = runPipe producerComp consumerComp

pipeTest :: TestTree
pipeTest = testCase
  "Mutually recursive pipe computation should run correctly" $
  do
    res <- returnVal $ runComp pipedComp idLift (mkEnvOps 5)
    assertEqual
      "Pipe computation should return 18"
      18 res

pipeTests :: TestTree
pipeTests = testGroup "PipeEff Tests"
  [
    pipeTest
  ]
