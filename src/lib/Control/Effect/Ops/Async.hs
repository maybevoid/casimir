
module Control.Effect.Ops.Async
where

import Data.Kind
import Control.Monad
import Control.Monad.Trans.Free
import Control.Concurrent.Async

import Control.Effect.Base
import Control.Effect.Free
import Control.Effect.Computation
import Control.Effect.Ops.Io

data AsyncEff (t :: Type -> Type)

data AsyncOps t eff = AsyncOps {
  awaitOp :: forall a . t a -> eff a,
  awaitAllOp :: forall a . [t a] -> eff [a]
}

data AsyncCoOp t r where
  AwaitOp :: forall t r a . t a -> (a -> r) -> AsyncCoOp t r
  AwaitAllOp :: forall t r a . [t a] -> ([a] -> r) -> AsyncCoOp t r

type AsyncConstraint t eff = (?asyncOps :: AsyncOps t eff)

instance Functor (AsyncCoOp t) where
  fmap f (AwaitOp task cont) = AwaitOp task $ f . cont
  fmap f (AwaitAllOp tasks cont) = AwaitAllOp tasks $ f . cont

instance EffFunctor (AsyncOps t) where
  effmap lift ops = AsyncOps {
    awaitOp = lift . (awaitOp ops),
    awaitAllOp = lift . (awaitAllOp ops)
  }

instance FreeOps (AsyncEff t) where
  type Operation (AsyncEff t) = AsyncOps t
  type CoOperation (AsyncEff t) = AsyncCoOp t

  mkFreeOps liftCoOp = AsyncOps {
    awaitOp = \task -> liftCoOp $ AwaitOp task id,
    awaitAllOp = \tasks -> liftCoOp $ AwaitAllOp tasks id
  }

instance EffOps (AsyncEff t) where
  type OpsConstraint (AsyncEff t) eff = AsyncConstraint t eff

  captureOps = ?asyncOps
  withOps ops comp = let ?asyncOps = ops in comp

await
  :: forall a t eff
   . (Effect eff, AsyncConstraint t eff)
  => t a
  -> eff a
await = awaitOp captureOps

awaitAll
  :: forall a t eff
   . (Effect eff, AsyncConstraint t eff)
  => [t a]
  -> eff [a]
awaitAll = awaitAllOp captureOps

handleAsync
  :: forall free eff1 eff2 a t
   . ( Effect eff1
     , Effect eff2
     , FreeEff free
     , OpsConstraint IoEff eff2
     )
  => (forall x . eff1 x -> IO x)
  -> (forall x
      . (AsyncConstraint t (FreeMonad (AsyncEff t) eff1))
     => t x
     -> FreeMonad (AsyncEff t) eff1 x)
  -> ((AsyncConstraint t (free (AsyncEff t) eff2))
      => free (AsyncEff t) eff2 a)
  -> eff2 a
handleAsync toIo taskRunner comp1 = withCoOpHandler @free handler2 comp1
 where
  handler2 :: CoOpHandler (AsyncEff t) a a eff2
  handler2 = CoOpHandler return handler3
   where
    handler3 :: AsyncCoOp t (eff2 a) -> eff2 a
    handler3 (AwaitOp task cont) = do
      x <- liftIo $ handleTask task
      cont x
    handler3 (AwaitAllOp tasks cont) = do
      xs <- liftIo $ mapConcurrently handleTask tasks
      cont xs

  handleTask :: forall b . t b -> IO b
  handleTask task = handleFreeT comp2
   where
    comp2 :: FreeT (AsyncCoOp t) eff1 b
    comp2 = unFreeT $ withOps @(AsyncEff t) freeOps $ taskRunner task

  handleFreeT :: forall b . FreeT (AsyncCoOp t) eff1 b -> IO b
  handleFreeT m = join $ toIo $ do
    f <- runFreeT m
    return $ handleFreeF f
   where
    handleFreeF :: FreeF (AsyncCoOp t) b (FreeT (AsyncCoOp t) eff1 b) -> IO b
    handleFreeF (Pure x) = toIo $ return x
    handleFreeF (Free coop) = handleAwait coop

    handleAwait :: AsyncCoOp t (FreeT (AsyncCoOp t) eff1 b) -> IO b
    handleAwait (AwaitOp task cont) = do
      x <- handleTask task
      handleFreeT $ cont x
    handleAwait (AwaitAllOp tasks cont) = do
      xs <- mapConcurrently handleTask tasks
      handleFreeT $ cont xs