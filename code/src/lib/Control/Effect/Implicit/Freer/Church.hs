
module Control.Effect.Implicit.Freer.Church
  ( FreerChurchMonad (..)
  )
where

import Control.Monad (ap)

import Control.Effect.Implicit.Base

import Control.Effect.Implicit.Freer.EffCoOp
import Control.Effect.Implicit.Freer.FreeOps
import Control.Effect.Implicit.Freer.FreerEff

newtype FreerChurchMonad ops eff a = FreerChurchMonad {
  runFreerChurchMonad :: forall r . FreerCoOpHandler ops a r eff -> eff r
}

instance
  (Monad eff, FreerOps ops)
  => Functor (FreerChurchMonad ops eff)
  where
    fmap = mapFreerChurchMonad
    {-# INLINE fmap #-}

instance
  (Monad eff, FreerOps ops)
  => Applicative (FreerChurchMonad ops eff)
  where
    pure = liftPure
    (<*>) = ap

instance
  (Monad eff, FreerOps ops)
  => Monad (FreerChurchMonad ops eff)
  where
    (>>=) = bindFreerChurchMonad
    {-# INLINE (>>=) #-}

instance
  FreerEff FreerChurchMonad
  where
    freerOps = churchOps
    liftFreer = liftFreerChurchMonad
    handleFreer handler eff = runFreerChurchMonad eff handler

liftFreerChurchMonad
  :: forall ops eff a .
  ( Effect eff
  , FreerOps ops
  )
  => eff a
  -> FreerChurchMonad ops eff a
liftFreerChurchMonad mx = FreerChurchMonad $
  \(FreerCoOpHandler handleReturn _) ->
   do
    x <- mx
    handleReturn x
{-# INLINE liftFreerChurchMonad #-}

liftChurchOps
  :: forall ops eff a .
  ( Effect eff
  , FreerOps ops
  )
  => FreerCoOp ops a
  -> FreerChurchMonad ops eff a
liftChurchOps ops = FreerChurchMonad cont
 where
  cont :: forall r . FreerCoOpHandler ops a r eff -> eff r
  cont (FreerCoOpHandler handleReturn handleCoOp) =
    handleCoOp $ CoOpCont ops handleReturn
{-# INLINE liftChurchOps #-}

churchOps
  :: forall ops eff .
  (FreerOps ops, Effect eff)
  => Operation ops (FreerChurchMonad ops eff)
churchOps = mkFreerOps liftChurchOps
{-# INLINE churchOps #-}

mapFreerChurchMonad
  :: forall ops eff a b .
  ( Effect eff
  , FreerOps ops
  )
  => (a -> b)
  -> FreerChurchMonad ops eff a
  -> FreerChurchMonad ops eff b
mapFreerChurchMonad f (FreerChurchMonad m1) = FreerChurchMonad m2
 where
  m2 :: forall r . FreerCoOpHandler ops b r eff -> eff r
  m2 (FreerCoOpHandler handleReturn handleCoOp) =
    m1 $ FreerCoOpHandler
      (handleReturn . f)
      handleCoOp
{-# INLINE mapFreerChurchMonad #-}

bindFreerChurchMonad
  :: forall ops eff a b .
  ( Effect eff
  , FreerOps ops
  )
  => FreerChurchMonad ops eff a
  -> (a -> FreerChurchMonad ops eff b)
  -> FreerChurchMonad ops eff b
bindFreerChurchMonad (FreerChurchMonad m1) cont1 = FreerChurchMonad m2
 where
  m2 :: forall r . FreerCoOpHandler ops b r eff -> eff r
  m2 handler1@(FreerCoOpHandler _ handleCoOp) =
    m1 handler2
     where
      handler2 :: FreerCoOpHandler ops a r eff
      handler2 = FreerCoOpHandler
        (\x -> runFreerChurchMonad (cont1 x) handler1)
        handleCoOp
{-# INLINE bindFreerChurchMonad #-}

liftPure
  :: forall ops eff a .
  ( Effect eff
  , FreerOps ops
  )
  => a
  -> FreerChurchMonad ops eff a
liftPure x = FreerChurchMonad $
  \(FreerCoOpHandler handleReturn _) ->
    handleReturn x
{-# INLINE liftPure #-}
