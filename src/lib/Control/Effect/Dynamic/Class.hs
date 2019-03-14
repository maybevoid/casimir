
module Control.Effect.Dynamic.Class
where

import Control.Effect.Base

newtype DynamicEff ops eff a = DynamicEff {
  runDynamicEff :: forall r . OpsHandler ops a r eff -> eff r
}

liftDynamicEff
  :: forall ops eff a .
  ( Effect eff
  , FreeOps ops
  )
  => eff a
  -> DynamicEff ops eff a
liftDynamicEff mx = DynamicEff $ \handler -> do
  x <- mx
  handleReturn handler x

liftDynamicOps
  :: forall ops eff a .
  ( Effect eff
  , FreeOps ops
  )
  => CoOperation ops a
  -> DynamicEff ops eff a
liftDynamicOps ops = DynamicEff $ cont
 where
  cont :: forall r . OpsHandler ops a r eff -> eff r
  cont handler = handleOps handler $ fmap (handleReturn handler) ops

dynamicOps
  :: forall ops eff .
  (EffOps ops, Effect eff)
  => Operation ops (DynamicEff ops eff)
dynamicOps = mkFreeOps liftDynamicOps

instance
  (Monad eff, EffOps ops)
  => Functor (DynamicEff ops eff)
  where
    fmap = mapDynamicEff

instance
  (Monad eff, EffOps ops)
  => Applicative (DynamicEff ops eff)
  where
    pure = liftPure
    mf <*> mx = do
      f <- mf
      x <- mx
      return $ f x

instance
  (Monad eff, EffOps ops)
  => Monad (DynamicEff ops eff)
  where
    (>>=) = bindDynamicEff

mapDynamicEff
  :: forall ops eff a b .
  ( Effect eff
  , EffOps ops
  )
  => (a -> b)
  -> DynamicEff ops eff a
  -> DynamicEff ops eff b
mapDynamicEff f (DynamicEff m1) = DynamicEff m2
 where
  m2 :: forall r . OpsHandler ops b r eff -> eff r
  m2 handler = m1 $ OpsHandler {
    handleReturn = \x -> handleReturn handler (f x),
    handleOps = handleOps handler
  }

bindDynamicEff
  :: forall ops eff a b .
  ( Effect eff
  , EffOps ops
  )
  => DynamicEff ops eff a
  -> (a -> DynamicEff ops eff b)
  -> DynamicEff ops eff b
bindDynamicEff (DynamicEff m1) cont1 = DynamicEff m2
 where
  m2 :: forall r . OpsHandler ops b r eff -> eff r
  m2 handler1 = m1 handler2
   where
    handler2 :: OpsHandler ops a r eff
    handler2 = OpsHandler {
      handleReturn = \x -> runDynamicEff (cont1 x) handler1,
      handleOps = handleOps handler1
    }

liftPure
  :: forall ops eff a .
  ( Effect eff
  , EffOps ops
  )
  => a
  -> DynamicEff ops eff a
liftPure x = DynamicEff $ \handler -> handleReturn handler x
