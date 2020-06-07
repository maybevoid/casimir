
module Casimir.Free.Monad.Church
  ( ChurchMonad (..)
  , withCoOpHandler
  , withCoOpHandlerAndOps
  )
where

import Control.Monad (ap)

import Casimir.Base
import Casimir.Free.CoOp
import Casimir.Free.FreeOps
import Casimir.Free.FreeEff

import qualified Casimir.Free.Handler as Handler

newtype ChurchMonad ops m a = ChurchMonad {
  runChurchMonad :: forall r . CoOpHandler ops a r m -> m r
}

withCoOpHandler
  :: forall handler m a r
   . ( Monad m
     , EffOps handler
     , FreeOps handler
     , ImplicitOps handler
     )
  => CoOpHandler handler a r m
  -> ((OpsConstraint handler (ChurchMonad handler m))
      => ChurchMonad handler m a)
  -> m r
withCoOpHandler = Handler.withCoOpHandler @ChurchMonad

withCoOpHandlerAndOps
  :: forall ops handler m a r
    . ( EffOps ops
      , EffOps handler
      , FreeOps handler
      , ImplicitOps ops
      , ImplicitOps handler
      , EffConstraint ops m
      , EffFunctor Lift (Operation ops)
      )
  => CoOpHandler handler a r m
  -> (( OpsConstraint handler (ChurchMonad handler m)
      , OpsConstraint ops (ChurchMonad handler m)
      )
      => ChurchMonad handler m a)
  -> m r
withCoOpHandlerAndOps = Handler.withCoOpHandlerAndOps @ChurchMonad @ops

instance
  (Monad m, FreeOps ops)
  => Functor (ChurchMonad ops m)
  where
    fmap = mapChurchMonad
    {-# INLINE fmap #-}

instance
  (Monad m, FreeOps ops)
  => Applicative (ChurchMonad ops m)
  where
    pure = liftPure
    (<*>) = ap

instance
  (Monad m, FreeOps ops)
  => Monad (ChurchMonad ops m)
  where
    (>>=) = bindChurchMonad
    {-# INLINE (>>=) #-}

instance
  FreeEff ChurchMonad
  where
    freeOps = churchOps
    liftFree = liftChurchMonad

instance
  FreeHandler ChurchMonad
   where
    handleFree handler m = runChurchMonad m handler

liftChurchMonad
  :: forall ops m a .
  ( Monad m
  , FreeOps ops
  )
  => m a
  -> ChurchMonad ops m a
liftChurchMonad mx = ChurchMonad $
  \(CoOpHandler handleReturn _) ->
   do
    x <- mx
    handleReturn x
{-# INLINE liftChurchMonad #-}

liftChurchOps
  :: forall ops m a .
  ( Monad m
  , FreeOps ops
  )
  => CoOperation ops a
  -> ChurchMonad ops m a
liftChurchOps ops = ChurchMonad cont
 where
  cont :: forall r . CoOpHandler ops a r m -> m r
  cont (CoOpHandler handleReturn handleCoOp) =
    handleCoOp $ fmap handleReturn ops
{-# INLINE liftChurchOps #-}

churchOps
  :: forall ops m .
  (FreeOps ops, Monad m)
  => Operation ops (ChurchMonad ops m)
churchOps = mkFreeOps liftChurchOps
{-# INLINE churchOps #-}

mapChurchMonad
  :: forall ops m a b .
  ( Monad m
  , FreeOps ops
  )
  => (a -> b)
  -> ChurchMonad ops m a
  -> ChurchMonad ops m b
mapChurchMonad f (ChurchMonad m1) = ChurchMonad m2
 where
  m2 :: forall r . CoOpHandler ops b r m -> m r
  m2 (CoOpHandler handleReturn handleCoOp) =
    m1 $ CoOpHandler
      (handleReturn . f)
      handleCoOp
{-# INLINE mapChurchMonad #-}

bindChurchMonad
  :: forall ops m a b .
  ( Monad m
  , FreeOps ops
  )
  => ChurchMonad ops m a
  -> (a -> ChurchMonad ops m b)
  -> ChurchMonad ops m b
bindChurchMonad (ChurchMonad m1) cont1 = ChurchMonad m2
 where
  m2 :: forall r . CoOpHandler ops b r m -> m r
  m2 handler1@(CoOpHandler _ handleCoOp) =
    m1 handler2
     where
      handler2 :: CoOpHandler ops a r m
      handler2 = CoOpHandler
        (\x -> runChurchMonad (cont1 x) handler1)
        handleCoOp
{-# INLINE bindChurchMonad #-}

liftPure
  :: forall ops m a .
  ( Monad m
  , FreeOps ops
  )
  => a
  -> ChurchMonad ops m a
liftPure x = ChurchMonad $
  \(CoOpHandler handleReturn _) ->
    handleReturn x
{-# INLINE liftPure #-}
