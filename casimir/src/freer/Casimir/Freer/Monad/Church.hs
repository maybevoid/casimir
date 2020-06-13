
module Casimir.Freer.Monad.Church
  ( ChurchMonad (..)
  )
where

import Control.Monad (ap)

import Casimir.Freer.FreeOps
import Casimir.Freer.FreeTransformer

newtype ChurchMonad ops m a = ChurchMonad {
  runChurchMonad :: forall r . CoOpHandler ops a r m -> m r
}

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
  FreeTransformer ChurchMonad
  where
    freeOps
      :: forall ops m
       . ( Monad m, FreeOps ops )
      => ops (ChurchMonad ops m)
    freeOps = mkFreeOps liftChurchOps

    liftFree = liftChurchMonad
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
    handleCoOp ops handleReturn
{-# INLINE liftChurchOps #-}

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
