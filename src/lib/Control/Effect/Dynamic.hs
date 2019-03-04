
module Control.Effect.Dynamic

where

import Control.Effect.Base
import Control.Effect.Computation

data OpsHandler f a r eff = OpsHandler {
  handleReturn :: a -> eff r,
  handleOps :: f (eff r) -> eff r
}

newtype DynamicEff f eff a = DynamicEff {
  runDynamicEff :: forall r . OpsHandler f a r eff -> eff r
}

type DynamicHandler ops1 ops2 a r eff =
  Computation ops1 (OpsHandler (CoOperation ops2) a r) eff

data DynamicContext ops1 ops2 ops3 a r eff =
  DynamicContext
    (DynamicHandler ops2 ops1 a r eff)
    (Computation (Union ops1 ops3) (Return a) eff)

class (EffOps ops) => DynamicOps ops where
  dynamicOps
    :: forall eff .
    (Effect eff)
    => Operation ops (DynamicEff (CoOperation ops) eff)

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
  m2 :: forall r . OpsHandler f b r eff -> eff r
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
  m2 :: forall r . OpsHandler f b r eff -> eff r
  m2 handler1 = m1 handler2
   where
    handler2 :: OpsHandler f a r eff
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
  cont :: forall r . OpsHandler f a r eff -> eff r
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
  => OpsHandler (CoOperation ops) a b eff
  -> (OpsConstraint ops (DynamicEff (CoOperation ops) eff)
      => (DynamicEff (CoOperation ops) eff) a)
  -> eff b
handleDynamic handler comp1 = runDynamicEff comp2 handler
 where
  ops :: Operation ops (DynamicEff (CoOperation ops) eff)
  ops = dynamicOps

  comp2 :: DynamicEff (CoOperation ops) eff a
  comp2 = bindConstraint ops comp1

mkDynamicHandler
  :: forall ops1 ops2 a r eff1 .
  ( EffOps ops1
  , EffOps ops2
  , Effect eff1
  )
  => (forall eff2 .
      (Effect eff2)
      => LiftEff eff1 eff2
      -> (OpsConstraint ops1 eff2
          => OpsHandler (CoOperation ops2) a r eff2))
  -> DynamicHandler ops1 ops2 a r eff1
mkDynamicHandler = Computation

genericDynamicHandler
  :: forall ops1 ops2 a r eff1.
  ( EffOps ops1
  , EffOps ops2
  , Effect eff1
  )
  => (forall eff2 .
      (Effect eff2, OpsConstraint ops1 eff2)
      => OpsHandler (CoOperation ops2) a r eff2)
  -> DynamicHandler ops1 ops2 a r eff1
genericDynamicHandler comp = mkDynamicHandler $ \_ -> comp

applyDynamic
  :: forall ops1 ops2 a r eff .
  ( EffOps ops1
  , EffOps ops2
  , DynamicOps ops1
  , Effect eff
  , OpsConstraint ops2 eff
  )
  => DynamicHandler ops2 ops1 a r eff
  -> Computation ops1 (Return a) eff
  -> eff r
applyDynamic handler1 comp1 = comp2
 where
  handler2 :: OpsHandler (CoOperation ops1) a r eff
  handler2 = runComp handler1 id

  comp2 :: eff r
  comp2 = handleDynamic handler2 $ returnVal $
    runComp comp1 liftReturn
