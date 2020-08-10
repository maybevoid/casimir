
module Casimir.Ops.Async
where

import Control.Concurrent.Async

import Casimir.Base
import Casimir.Freer

import Casimir.Ops.Io

data AsyncTag

data AsyncOps t m = AsyncOps
  { awaitOp :: forall a . t a -> m a
  , awaitAllOp :: forall a . [t a] -> m [a]
  }

data AsyncCoOp t r where
  AwaitOp :: forall t a . t a -> AsyncCoOp t a
  AwaitAllOp :: forall t a . [t a] -> AsyncCoOp t [a]

instance EffFunctor Lift (AsyncOps t) where
  effmap (Lift lift) ops = AsyncOps {
    awaitOp = lift . (awaitOp ops),
    awaitAllOp = lift . (awaitAllOp ops)
  }

instance FreeOps (AsyncOps t) where
  type CoOperation (AsyncOps t) = AsyncCoOp t

  mkFreeOps liftCoOp = AsyncOps {
    awaitOp = \task -> liftCoOp $ AwaitOp task,
    awaitAllOp = \tasks -> liftCoOp $ AwaitAllOp tasks
  }

instance HasLabel (AsyncOps t) where
  type GetLabel (AsyncOps t) = Tag AsyncTag

await
  :: forall a t
   . t a
  -> Eff '[AsyncOps t] a
await = awaitOp captureOp

awaitAll
  :: forall a t
   . [t a]
  -> Eff '[AsyncOps t] [a]
awaitAll = awaitAllOp captureOp

handleAsync
  :: forall free m a t
   . ( FreeTransformer free
     , EffConstraint '[IoOps] m
     )
  => (forall x
      . (EffConstraint '[AsyncOps t] (free (Multi '[AsyncOps t]) IO))
     => t x
     -> free (Multi '[AsyncOps t]) IO x)
  -> ((EffConstraint '[AsyncOps t] (free (Multi '[AsyncOps t]) m))
      => free (Multi '[AsyncOps t]) m a)
  -> m a
handleAsync taskRunner comp1 =
  withCoOpHandler @free @(Multi '[AsyncOps t]) handler2 comp1
   where
    handler2 :: CoOpHandler (Multi '[AsyncOps t]) a a m
    handler2 = CoOpHandler return handler3
     where
      handler3 :: forall x . UnionCoOp (AsyncCoOp t) NoCoOp x -> (x -> m a) -> m a
      handler3 (LeftOp op) = handler4 op

      handler4 :: forall x . AsyncCoOp t x -> (x -> m a) -> m a
      handler4 (AwaitOp task) cont = do
        x <- liftIo $ handleTask task
        cont x
      handler4 (AwaitAllOp tasks) cont = do
        xs <- liftIo $ mapConcurrently handleTask tasks
        cont xs

    handleTask :: forall b . t b -> IO b
    handleTask task =
      withCoOpHandler @free @(Multi '[AsyncOps t]) handler4 $ taskRunner task
       where
        handler4 :: CoOpHandler (Multi '[AsyncOps t]) b b IO
        handler4 = CoOpHandler return handler5

        handler5 :: forall x . UnionCoOp (AsyncCoOp t) NoCoOp x -> (x -> (IO b)) -> IO b
        handler5 (LeftOp op) = handleAwait op

        handleAwait :: forall x . AsyncCoOp t x -> (x -> (IO b)) -> IO b
        handleAwait (AwaitOp task') cont = do
          x <- handleTask task'
          cont x
        handleAwait (AwaitAllOp tasks) cont = do
          xs <- mapConcurrently handleTask tasks
          cont xs
