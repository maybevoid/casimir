
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

newtype ChurchMonad ops eff a = ChurchMonad {
  runChurchMonad :: forall r . CoOpHandler ops a r eff -> eff r
}

withCoOpHandler
  :: forall handler eff a r
   . ( Monad eff
     , EffOps handler
     , FreeOps handler
     , ImplicitOps handler
     )
  => CoOpHandler handler a r eff
  -> ((OpsConstraint handler (ChurchMonad handler eff))
      => ChurchMonad handler eff a)
  -> eff r
withCoOpHandler = Handler.withCoOpHandler @ChurchMonad

withCoOpHandlerAndOps
  :: forall ops handler eff a r
    . ( EffOps ops
      , EffOps handler
      , FreeOps handler
      , ImplicitOps ops
      , ImplicitOps handler
      , EffConstraint ops eff
      , EffFunctor Lift (Operation ops)
      )
  => CoOpHandler handler a r eff
  -> (( OpsConstraint handler (ChurchMonad handler eff)
      , OpsConstraint ops (ChurchMonad handler eff)
      )
      => ChurchMonad handler eff a)
  -> eff r
withCoOpHandlerAndOps = Handler.withCoOpHandlerAndOps @ChurchMonad @ops

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

instance
  FreeHandler ChurchMonad
   where
    handleFree handler eff = runChurchMonad eff handler

liftChurchMonad
  :: forall ops eff a .
  ( Monad eff
  , FreeOps ops
  )
  => eff a
  -> ChurchMonad ops eff a
liftChurchMonad mx = ChurchMonad $
  \(CoOpHandler handleReturn _) ->
   do
    x <- mx
    handleReturn x
{-# INLINE liftChurchMonad #-}

liftChurchOps
  :: forall ops eff a .
  ( Monad eff
  , FreeOps ops
  )
  => CoOperation ops a
  -> ChurchMonad ops eff a
liftChurchOps ops = ChurchMonad cont
 where
  cont :: forall r . CoOpHandler ops a r eff -> eff r
  cont (CoOpHandler handleReturn handleCoOp) =
    handleCoOp $ fmap handleReturn ops
{-# INLINE liftChurchOps #-}

churchOps
  :: forall ops eff .
  (FreeOps ops, Monad eff)
  => Operation ops (ChurchMonad ops eff)
churchOps = mkFreeOps liftChurchOps
{-# INLINE churchOps #-}

mapChurchMonad
  :: forall ops eff a b .
  ( Monad eff
  , FreeOps ops
  )
  => (a -> b)
  -> ChurchMonad ops eff a
  -> ChurchMonad ops eff b
mapChurchMonad f (ChurchMonad m1) = ChurchMonad m2
 where
  m2 :: forall r . CoOpHandler ops b r eff -> eff r
  m2 (CoOpHandler handleReturn handleCoOp) =
    m1 $ CoOpHandler
      (handleReturn . f)
      handleCoOp
{-# INLINE mapChurchMonad #-}

bindChurchMonad
  :: forall ops eff a b .
  ( Monad eff
  , FreeOps ops
  )
  => ChurchMonad ops eff a
  -> (a -> ChurchMonad ops eff b)
  -> ChurchMonad ops eff b
bindChurchMonad (ChurchMonad m1) cont1 = ChurchMonad m2
 where
  m2 :: forall r . CoOpHandler ops b r eff -> eff r
  m2 handler1@(CoOpHandler _ handleCoOp) =
    m1 handler2
     where
      handler2 :: CoOpHandler ops a r eff
      handler2 = CoOpHandler
        (\x -> runChurchMonad (cont1 x) handler1)
        handleCoOp
{-# INLINE bindChurchMonad #-}

liftPure
  :: forall ops eff a .
  ( Monad eff
  , FreeOps ops
  )
  => a
  -> ChurchMonad ops eff a
liftPure x = ChurchMonad $
  \(CoOpHandler handleReturn _) ->
    handleReturn x
{-# INLINE liftPure #-}
