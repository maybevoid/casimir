
module Control.Effect.Free.Dynamic
where

import Control.Effect.Base

newtype DynamicMonad ops eff a = DynamicMonad {
  runDynamicMonad :: forall r . OpsHandler ops a r eff -> eff r
}

liftDynamicMonad
  :: forall ops eff a .
  ( Effect eff
  , FreeOps ops
  )
  => eff a
  -> DynamicMonad ops eff a
liftDynamicMonad mx = DynamicMonad $ \handler -> do
  x <- mx
  handleReturn handler x

liftDynamicOps
  :: forall ops eff a .
  ( Effect eff
  , FreeOps ops
  )
  => CoOperation ops a
  -> DynamicMonad ops eff a
liftDynamicOps ops = DynamicMonad $ cont
 where
  cont :: forall r . OpsHandler ops a r eff -> eff r
  cont handler = handleOps handler $ fmap (handleReturn handler) ops

dynamicOps
  :: forall ops eff .
  (FreeOps ops, Effect eff)
  => Operation ops (DynamicMonad ops eff)
dynamicOps = mkFreeOps liftDynamicOps

instance
  (Monad eff, FreeOps ops)
  => Functor (DynamicMonad ops eff)
  where
    fmap = mapDynamicMonad

instance
  (Monad eff, FreeOps ops)
  => Applicative (DynamicMonad ops eff)
  where
    pure = liftPure
    mf <*> mx = do
      f <- mf
      x <- mx
      return $ f x

instance
  (Monad eff, FreeOps ops)
  => Monad (DynamicMonad ops eff)
  where
    (>>=) = bindDynamicMonad

instance
  FreeEff DynamicMonad
  where
    freeOps = dynamicOps
    liftFree = liftDynamicMonad
    handleFree handler eff = runDynamicMonad eff handler

mapDynamicMonad
  :: forall ops eff a b .
  ( Effect eff
  , FreeOps ops
  )
  => (a -> b)
  -> DynamicMonad ops eff a
  -> DynamicMonad ops eff b
mapDynamicMonad f (DynamicMonad m1) = DynamicMonad m2
 where
  m2 :: forall r . OpsHandler ops b r eff -> eff r
  m2 handler = m1 $ OpsHandler {
    handleReturn = \x -> handleReturn handler (f x),
    handleOps = handleOps handler
  }

bindDynamicMonad
  :: forall ops eff a b .
  ( Effect eff
  , FreeOps ops
  )
  => DynamicMonad ops eff a
  -> (a -> DynamicMonad ops eff b)
  -> DynamicMonad ops eff b
bindDynamicMonad (DynamicMonad m1) cont1 = DynamicMonad m2
 where
  m2 :: forall r . OpsHandler ops b r eff -> eff r
  m2 handler1 = m1 handler2
   where
    handler2 :: OpsHandler ops a r eff
    handler2 = OpsHandler {
      handleReturn = \x -> runDynamicMonad (cont1 x) handler1,
      handleOps = handleOps handler1
    }

liftPure
  :: forall ops eff a .
  ( Effect eff
  , FreeOps ops
  )
  => a
  -> DynamicMonad ops eff a
liftPure x = DynamicMonad $ \handler -> handleReturn handler x
