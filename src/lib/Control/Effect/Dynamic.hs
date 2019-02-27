
module Control.Effect.Dynamic

where

import Control.Effect.Class

data OpsHandler f eff a r = OpsHandler {
  handleReturn :: a -> eff r,
  handleOps :: f (eff r) -> eff r
}

newtype DynamicEff f eff a = DynamicEff {
  runDynamicEff :: forall r . OpsHandler f eff a r -> eff r
}

class (EffOps ops) => DynamicOps ops where
  dynamicOps
    :: forall eff .
    (Effect eff)
    => ops (DynamicEff (FreeModel ops) eff)

instance (Monad eff, Functor f) => Functor (DynamicEff f eff) where
  fmap = mapDynamicEff

instance (Monad eff, Functor f) => Applicative (DynamicEff f eff) where
  pure = liftPure
  mf <*> mx = do
    f <- mf
    x <- mx
    return $ f x

instance (Monad eff, Functor f) => Monad (DynamicEff f eff) where
  (>>=) = bindDynamicEff

mapDynamicEff
  :: forall f eff a b .
  ( Functor f
  , Effect eff
  )
  => (a -> b)
  -> DynamicEff f eff a
  -> DynamicEff f eff b
mapDynamicEff f (DynamicEff m1) = DynamicEff m2
 where
  m2 :: forall r . OpsHandler f eff b r -> eff r
  m2 handler = m1 $ OpsHandler {
    handleReturn = \x -> handleReturn handler (f x),
    handleOps = handleOps handler
  }

bindDynamicEff
  :: forall f eff a b .
  ( Functor f
  , Effect eff
  )
  => DynamicEff f eff a
  -> (a -> DynamicEff f eff b)
  -> DynamicEff f eff b
bindDynamicEff (DynamicEff m1) cont1 = DynamicEff m2
 where
  m2 :: forall r . OpsHandler f eff b r -> eff r
  m2 handler1 = m1 handler2
   where
    handler2 :: OpsHandler f eff a r
    handler2 = OpsHandler {
      handleReturn = \x -> runDynamicEff (cont1 x) handler1,
      handleOps = handleOps handler1
    }

liftPure
  :: forall f eff a .
  ( Functor f
  , Effect eff
  )
  => a
  -> DynamicEff f eff a
liftPure x = DynamicEff $ \handler -> handleReturn handler x

liftReturn
  :: forall f eff a .
  ( Functor f
  , Effect eff
  )
  => eff a
  -> DynamicEff f eff a
liftReturn mx = DynamicEff $ \handler -> do
  x <- mx
  handleReturn handler x

liftOps
  :: forall f eff a .
  ( Functor f
  , Effect eff
  )
  => f (eff a)
  -> DynamicEff f eff a
liftOps ops = DynamicEff $ cont
 where
  cont :: forall r . OpsHandler f eff a r -> eff r
  cont handler = handleOps handler $ fmap mapper ops
   where
    mapper :: eff a -> eff r
    mapper mx = do
      x <- mx
      handleReturn handler x

handleDynamic
  :: forall ops eff a b.
  ( Effect eff
  , EffOps ops
  , DynamicOps ops
  )
  => OpsHandler (FreeModel ops) eff a b
  -> (EffConstraint ops (DynamicEff (FreeModel ops) eff)
      => (DynamicEff (FreeModel ops) eff) a)
  -> eff b
handleDynamic handler comp1 = runDynamicEff comp2 handler
 where
  ops :: ops (DynamicEff (FreeModel ops) eff)
  ops = dynamicOps

  comp2 :: DynamicEff (FreeModel ops) eff a
  comp2 = bindConstraint ops comp1