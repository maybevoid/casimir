
module Casimir.Ops.Async
where

import Data.Kind
import Control.Concurrent.Async

import Casimir.Base
import Casimir.Free

import Casimir.Ops.Io

data AsyncEff (t :: Type -> Type)

data AsyncOps t m = AsyncOps {
  awaitOp :: forall a . t a -> m a,
  awaitAllOp :: forall a . [t a] -> m [a]
}

data AsyncCoOp t r where
  AwaitOp :: forall t r a . t a -> (a -> r) -> AsyncCoOp t r
  AwaitAllOp :: forall t r a . [t a] -> ([a] -> r) -> AsyncCoOp t r

instance EffOps (AsyncEff t) where
  type Operation (AsyncEff t) = AsyncOps t

instance EffCoOp (AsyncEff t) where
  type CoOperation (AsyncEff t) = AsyncCoOp t

instance Functor (AsyncCoOp t) where
  fmap f (AwaitOp task cont) = AwaitOp task $ f . cont
  fmap f (AwaitAllOp tasks cont) = AwaitAllOp tasks $ f . cont

instance EffFunctor Lift (AsyncOps t) where
  mmap (Lift lift) ops = AsyncOps {
    awaitOp = lift . (awaitOp ops),
    awaitAllOp = lift . (awaitAllOp ops)
  }

instance FreeOps (AsyncEff t) where
  mkFreeOps liftCoOp = AsyncOps {
    awaitOp = \task -> liftCoOp $ AwaitOp task id,
    awaitAllOp = \tasks -> liftCoOp $ AwaitAllOp tasks id
  }

type AsyncConstraint t m =
  (?_Control_Monad_Implicit_Ops_Async_asyncOps :: AsyncOps t m)

instance ImplicitOps (AsyncEff t) where
  type OpsConstraint (AsyncEff t) m = AsyncConstraint t m

  captureOps =
    ?_Control_Monad_Implicit_Ops_Async_asyncOps

  withOps ops comp =
    let
      ?_Control_Monad_Implicit_Ops_Async_asyncOps
        = ops in comp

await
  :: forall a t m
   . (Monad m, AsyncConstraint t m)
  => t a
  -> m a
await = awaitOp captureOps

awaitAll
  :: forall a t m
   . (Monad m, AsyncConstraint t m)
  => [t a]
  -> m [a]
awaitAll = awaitAllOp captureOps

handleAsync
  :: forall free m a t
   . ( FreeHandler free
     , EffConstraint IoEff m
     )
  => (forall x
      . (AsyncConstraint t (free (AsyncEff t) IO))
     => t x
     -> free (AsyncEff t) IO x)
  -> ((AsyncConstraint t (free (AsyncEff t) m))
      => free (AsyncEff t) m a)
  -> m a
handleAsync taskRunner comp1 =
  withCoOpHandler @free handler2 comp1
   where
    handler2 :: CoOpHandler (AsyncEff t) a a m
    handler2 = CoOpHandler return handler3
     where
      handler3 :: AsyncCoOp t (m a) -> m a
      handler3 (AwaitOp task cont) = do
        x <- liftIo $ handleTask task
        cont x
      handler3 (AwaitAllOp tasks cont) = do
        xs <- liftIo $ mapConcurrently handleTask tasks
        cont xs

    handleTask :: forall b . t b -> IO b
    handleTask task =
      withCoOpHandler @free handler4 $ taskRunner task
       where
        handler4 :: CoOpHandler (AsyncEff t) b b IO
        handler4 = CoOpHandler return handleAwait

        handleAwait :: AsyncCoOp t (IO b) -> IO b
        handleAwait (AwaitOp task' cont) = do
          x <- handleTask task'
          cont x
        handleAwait (AwaitAllOp tasks cont) = do
          xs <- mapConcurrently handleTask tasks
          cont xs
