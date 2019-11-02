
module Control.Effect.Implicit.Freer.ChurchMonad
  ( ChurchMonad (..)
  )
where

import Control.Monad (ap)

import Control.Effect.Implicit.Base

import Control.Effect.Implicit.Freer.EffCoOp
import Control.Effect.Implicit.Freer.FreeOps
import Control.Effect.Implicit.Freer.FreeEff

newtype ChurchMonad ops eff a = ChurchMonad {
  runChurchMonad :: forall r . FreerCoOpHandler ops a r eff -> eff r
}

instance
  (Monad eff, FreeOps ops)
  => Functor (ChurchMonad ops eff)
  where
    fmap = mapChurchMonad
    {-# INLINE fmap #-}

instance
  (Monad eff, FreeOps ops)
  => Applicative (ChurchMonad ops eff)
  where
    pure = liftPure
    (<*>) = ap

instance
  (Monad eff, FreeOps ops)
  => Monad (ChurchMonad ops eff)
  where
    (>>=) = bindChurchMonad
    {-# INLINE (>>=) #-}

instance
  FreeEff ChurchMonad
  where
    freeOps = churchOps
    liftFree = liftChurchMonad
    handleFree handler eff = runChurchMonad eff handler

liftChurchMonad
  :: forall ops eff a .
  ( Effect eff
  , FreeOps ops
  )
  => eff a
  -> ChurchMonad ops eff a
liftChurchMonad mx = ChurchMonad $
  \(FreerCoOpHandler handleReturn _) ->
   do
    x <- mx
    handleReturn x
{-# INLINE liftChurchMonad #-}

liftChurchOps
  :: forall ops eff a .
  ( Effect eff
  , FreeOps ops
  )
  => CoOperation ops a
  -> ChurchMonad ops eff a
liftChurchOps ops = ChurchMonad cont
 where
  cont :: forall r . FreerCoOpHandler ops a r eff -> eff r
  cont (FreerCoOpHandler handleReturn handleCoOp) =
    handleCoOp $ CoOpCont ops handleReturn
{-# INLINE liftChurchOps #-}

churchOps
  :: forall ops eff .
  (FreeOps ops, Effect eff)
  => Operation ops (ChurchMonad ops eff)
churchOps = mkFreeOps liftChurchOps
{-# INLINE churchOps #-}

mapChurchMonad
  :: forall ops eff a b .
  ( Effect eff
  , FreeOps ops
  )
  => (a -> b)
  -> ChurchMonad ops eff a
  -> ChurchMonad ops eff b
mapChurchMonad f (ChurchMonad m1) = ChurchMonad m2
 where
  m2 :: forall r . FreerCoOpHandler ops b r eff -> eff r
  m2 (FreerCoOpHandler handleReturn handleCoOp) =
    m1 $ FreerCoOpHandler
      (handleReturn . f)
      handleCoOp
{-# INLINE mapChurchMonad #-}

bindChurchMonad
  :: forall ops eff a b .
  ( Effect eff
  , FreeOps ops
  )
  => ChurchMonad ops eff a
  -> (a -> ChurchMonad ops eff b)
  -> ChurchMonad ops eff b
bindChurchMonad (ChurchMonad m1) cont1 = ChurchMonad m2
 where
  m2 :: forall r . FreerCoOpHandler ops b r eff -> eff r
  m2 handler1@(FreerCoOpHandler _ handleCoOp) =
    m1 handler2
     where
      handler2 :: FreerCoOpHandler ops a r eff
      handler2 = FreerCoOpHandler
        (\x -> runChurchMonad (cont1 x) handler1)
        handleCoOp
{-# INLINE bindChurchMonad #-}

liftPure
  :: forall ops eff a .
  ( Effect eff
  , FreeOps ops
  )
  => a
  -> ChurchMonad ops eff a
liftPure x = ChurchMonad $
  \(FreerCoOpHandler handleReturn _) ->
    handleReturn x
{-# INLINE liftPure #-}
