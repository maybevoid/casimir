
module Casimir.Ops.Async
where

import Data.Kind
import Control.Concurrent.Async

import Casimir.Base
import Casimir.Free

import Casimir.Ops.Io

data AsyncEff (t :: Type -> Type)

data AsyncOps t eff = AsyncOps {
  awaitOp :: forall a . t a -> eff a,
  awaitAllOp :: forall a . [t a] -> eff [a]
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

instance EffFunctor (AsyncOps t) where
  effmap lift ops = AsyncOps {
    awaitOp = lift . (awaitOp ops),
    awaitAllOp = lift . (awaitAllOp ops)
  }

instance FreeOps (AsyncEff t) where
  mkFreeOps liftCoOp = AsyncOps {
    awaitOp = \task -> liftCoOp $ AwaitOp task id,
    awaitAllOp = \tasks -> liftCoOp $ AwaitAllOp tasks id
  }

type AsyncConstraint t eff =
  (?_Control_Effect_Implicit_Ops_Async_asyncOps :: AsyncOps t eff)

instance ImplicitOps (AsyncEff t) where
  type OpsConstraint (AsyncEff t) eff = AsyncConstraint t eff

  captureOps =
    ?_Control_Effect_Implicit_Ops_Async_asyncOps

  withOps ops comp =
    let
      ?_Control_Effect_Implicit_Ops_Async_asyncOps
        = ops in comp

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
  :: forall free eff a t
   . ( FreeHandler free
     , EffConstraint IoEff eff
     )
  => (forall x
      . (AsyncConstraint t (free (AsyncEff t) IO))
     => t x
     -> free (AsyncEff t) IO x)
  -> ((AsyncConstraint t (free (AsyncEff t) eff))
      => free (AsyncEff t) eff a)
  -> eff a
handleAsync taskRunner comp1 =
  withCoOpHandler @free handler2 comp1
   where
    handler2 :: CoOpHandler (AsyncEff t) a a eff
    handler2 = CoOpHandler return handler3
     where
      handler3 :: AsyncCoOp t (eff a) -> eff a
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
