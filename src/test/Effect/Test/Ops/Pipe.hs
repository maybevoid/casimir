{-# LANGUAGE DeriveFunctor #-}

module Effect.Test.Ops.Pipe where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Trans.Free
import Control.Monad.Trans.Class

import Control.Effect.Implicit
import Control.Effect.Implicit.Ops.Env

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

type YieldConstraint a eff = (?yieldOps :: YieldOps a eff)
type AwaitConstraint a eff = (?awaitOps :: AwaitOps a eff)

instance EffFunctor (YieldOps a) where
  effmap lifter ops = YieldOps $
    \x -> lifter $ yieldOp ops x

instance EffFunctor (AwaitOps a) where
  effmap lifter ops = AwaitOps $
    lifter $ awaitOp ops

instance FreeOps (YieldEff a) where
  type Operation (YieldEff a) = YieldOps a
  type CoOperation (YieldEff a) = YieldCoOp a

  mkFreeOps liftCoOp = YieldOps $
    \x -> liftCoOp $ YieldOp x id

instance FreeOps (AwaitEff a) where
  type Operation (AwaitEff a) = AwaitOps a
  type CoOperation (AwaitEff a) = AwaitCoOp a

  mkFreeOps liftCoOp = AwaitOps $
    liftCoOp $ AwaitOp id

instance EffOps (YieldEff a) where
  type OpsConstraint (YieldEff a) eff = YieldConstraint a eff

  withOps yieldOps comp
    = let ?yieldOps = yieldOps in comp

  captureOps = ?yieldOps

instance EffOps (AwaitEff a) where
  type OpsConstraint (AwaitEff a) eff = AwaitConstraint a eff

  withOps awaitOps comp
    = let ?awaitOps = awaitOps in comp

  captureOps = ?awaitOps

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
  => Computation ((YieldEff a) ∪ ops) (Return r) eff1
  -> Computation ((AwaitEff a) ∪ ops) (Return r) eff1
  -> Computation ops (Return r) eff1
runPipe producer1 consumer1 = Computation comp
   where
    comp :: forall eff2 . (Effect eff2)
      => LiftEff eff1 eff2
      -> Operation ops eff2
      -> Return r eff2
    comp lifter ops = Return $ pipe producer2 consumer2
     where
      producer2 :: FreeT (YieldCoOp a) eff2 r
      producer2 = returnVal $ runComp producer1
        (joinLift lifter (mkLiftEff lift)) $
        (mkFreeOps liftF) ∪ (effmap lift ops)

      consumer2 :: FreeT (AwaitCoOp a) eff2 r
      consumer2 = returnVal $ runComp consumer1
        (joinLift lifter (mkLiftEff lift)) $
        (mkFreeOps liftF) ∪ (effmap lift ops)

pipe
  :: forall a r eff
   . (Effect eff)
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
   . (Effect eff)
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
  :: forall a .
  GenericReturn ((YieldEff Int) ∪ (EnvEff Int)) a
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
  :: GenericReturn ((AwaitEff Int) ∪ (EnvEff Int)) Int
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
    res <- returnVal $ runComp pipedComp idLift (mkEnvOps 5)
    assertEqual
      "Pipe computation should return 18"
      18 res

pipeTests :: TestTree
pipeTests = testGroup "PipeEff Tests"
  [
    pipeTest
  ]