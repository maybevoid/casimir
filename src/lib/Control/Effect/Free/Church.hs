
module Control.Effect.Free.Church
  ( ChurchMonad (..)
  , churchLiftEff
  )
where

import Control.Monad (ap)
import Control.Effect.Base

newtype ChurchMonad ops eff a = ChurchMonad {
  runChurchMonad :: forall r . CoOpHandler ops a r eff -> eff r
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
liftChurchMonad mx = ChurchMonad $ \handler -> do
  x <- mx
  handleReturn handler x
{-# INLINE liftChurchMonad #-}

churchLiftEff
  :: forall ops eff
   . ( Effect eff
     , FreeOps ops
     )
  => LiftEff eff (ChurchMonad ops eff)
churchLiftEff = mkLiftEff liftChurchMonad
{-# INLINE churchLiftEff #-}

liftChurchOps
  :: forall ops eff a .
  ( Effect eff
  , FreeOps ops
  )
  => CoOperation ops a
  -> ChurchMonad ops eff a
liftChurchOps ops = ChurchMonad $ cont
 where
  cont :: forall r . CoOpHandler ops a r eff -> eff r
  cont handler = handleCoOp handler $ fmap (handleReturn handler) ops
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
  m2 :: forall r . CoOpHandler ops b r eff -> eff r
  m2 handler = m1 $ CoOpHandler {
    handleReturn = \x -> handleReturn handler (f x),
    handleCoOp = handleCoOp handler
  }
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
  m2 :: forall r . CoOpHandler ops b r eff -> eff r
  m2 handler1 = m1 handler2
   where
    handler2 :: CoOpHandler ops a r eff
    handler2 = CoOpHandler {
      handleReturn = \x -> runChurchMonad (cont1 x) handler1,
      handleCoOp = handleCoOp handler1
    }
{-# INLINE bindChurchMonad #-}

liftPure
  :: forall ops eff a .
  ( Effect eff
  , FreeOps ops
  )
  => a
  -> ChurchMonad ops eff a
liftPure x = ChurchMonad $ \handler -> handleReturn handler x
{-# INLINE liftPure #-}