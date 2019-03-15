
module Control.Effect.Free.Church
where

import Control.Effect.Base

newtype ChurchMonad ops eff a = ChurchMonad {
  runChurchMonad :: forall r . OpsHandler ops a r eff -> eff r
}

instance
  (Monad eff, FreeOps ops)
  => Functor (ChurchMonad ops eff)
  where
    fmap = mapChurchMonad

instance
  (Monad eff, FreeOps ops)
  => Applicative (ChurchMonad ops eff)
  where
    pure = liftPure
    mf <*> mx = do
      f <- mf
      x <- mx
      return $ f x

instance
  (Monad eff, FreeOps ops)
  => Monad (ChurchMonad ops eff)
  where
    (>>=) = bindChurchMonad

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

liftChurchOps
  :: forall ops eff a .
  ( Effect eff
  , FreeOps ops
  )
  => CoOperation ops a
  -> ChurchMonad ops eff a
liftChurchOps ops = ChurchMonad $ cont
 where
  cont :: forall r . OpsHandler ops a r eff -> eff r
  cont handler = handleOps handler $ fmap (handleReturn handler) ops

churchOps
  :: forall ops eff .
  (FreeOps ops, Effect eff)
  => Operation ops (ChurchMonad ops eff)
churchOps = mkFreeOps liftChurchOps

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
  m2 :: forall r . OpsHandler ops b r eff -> eff r
  m2 handler = m1 $ OpsHandler {
    handleReturn = \x -> handleReturn handler (f x),
    handleOps = handleOps handler
  }

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
  m2 :: forall r . OpsHandler ops b r eff -> eff r
  m2 handler1 = m1 handler2
   where
    handler2 :: OpsHandler ops a r eff
    handler2 = OpsHandler {
      handleReturn = \x -> runChurchMonad (cont1 x) handler1,
      handleOps = handleOps handler1
    }

liftPure
  :: forall ops eff a .
  ( Effect eff
  , FreeOps ops
  )
  => a
  -> ChurchMonad ops eff a
liftPure x = ChurchMonad $ \handler -> handleReturn handler x
