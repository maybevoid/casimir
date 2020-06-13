
module Casimir.Ops.Async
where

import Data.Kind
import Control.Concurrent.Async

import Casimir.Base
import Casimir.Freer

import Casimir.Ops.Io

data AsyncTag
data AsyncEff (t :: Type -> Type)

data AsyncOps t m = AsyncOps
  { awaitOp :: forall a . t a -> m a
  , awaitAllOp :: forall a . [t a] -> m [a]
  }

data AsyncCoOp t r where
  AwaitOp :: forall t r a . t a -> AsyncCoOp t a
  AwaitAllOp :: forall t r a . [t a] -> AsyncCoOp t [a]

instance Effect (AsyncEff t) where
  type Operation (AsyncEff t) = AsyncOps t

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
  -> Eff '[AsyncEff t] a
await = awaitOp captureOp

awaitAll
  :: forall a t m
   . [t a]
  -> Eff '[AsyncEff t] [a]
awaitAll = awaitAllOp captureOp

handleAsync
  :: forall free m a t
   . ( FreeTransformer free
     , EffConstraint '[IoEff] m
     )
  => (forall x
      . (OpsConstraint '[AsyncEff t] (free (AsyncOps t) IO))
     => t x
     -> free (AsyncOps t) IO x)
  -> ((OpsConstraint '[AsyncEff t] (free (AsyncOps t) m))
      => free (AsyncOps t) m a)
  -> m a
handleAsync taskRunner comp1 =
  withCoOpHandler @free @'[AsyncEff t] handler2 comp1
   where
    handler2 :: CoOpHandler (AsyncOps t) a a m
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
      withCoOpHandler @free @'[AsyncEff t] handler4 $ taskRunner task
       where
        handler4 :: CoOpHandler (AsyncOps t) b b IO
        handler4 = CoOpHandler return handleAwait

        handleAwait :: AsyncCoOp t (IO b) -> IO b
        handleAwait (AwaitOp task' cont) = do
          x <- handleTask task'
          cont x
        handleAwait (AwaitAllOp tasks cont) = do
          xs <- mapConcurrently handleTask tasks
          cont xs
