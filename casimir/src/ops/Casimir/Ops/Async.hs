
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

instance Effects (AsyncEff t) where
  type Operations (AsyncEff t) = AsyncOps t

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
  -> Eff (AsyncEff t) a
await = awaitOp captureOps

awaitAll
  :: forall a t m
   . [t a]
  -> Eff (AsyncEff t) [a]
awaitAll = awaitAllOp captureOps

-- handleAsync
--   :: forall free m a t
--    . ( FreeHandler free
--      , EffConstraint IoEff m
--      )
--   => (forall x
--       . (AsyncConstraint t (free (AsyncEff t) IO))
--      => t x
--      -> free (AsyncEff t) IO x)
--   -> ((AsyncConstraint t (free (AsyncEff t) m))
--       => free (AsyncEff t) m a)
--   -> m a
-- handleAsync taskRunner comp1 =
--   withCoOpHandler @free handler2 comp1
--    where
--     handler2 :: CoOpHandler (AsyncEff t) a a m
--     handler2 = CoOpHandler return handler3
--      where
--       handler3 :: AsyncCoOp t (m a) -> m a
--       handler3 (AwaitOp task cont) = do
--         x <- liftIo $ handleTask task
--         cont x
--       handler3 (AwaitAllOp tasks cont) = do
--         xs <- liftIo $ mapConcurrently handleTask tasks
--         cont xs

--     handleTask :: forall b . t b -> IO b
--     handleTask task =
--       withCoOpHandler @free handler4 $ taskRunner task
--        where
--         handler4 :: CoOpHandler (AsyncEff t) b b IO
--         handler4 = CoOpHandler return handleAwait

--         handleAwait :: AsyncCoOp t (IO b) -> IO b
--         handleAwait (AwaitOp task' cont) = do
--           x <- handleTask task'
--           cont x
--         handleAwait (AwaitAllOp tasks cont) = do
--           xs <- mapConcurrently handleTask tasks
--           cont xs
