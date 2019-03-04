
module Control.Effect.Dynamic

where

import Control.Effect.Base
import Control.Effect.Old.Computation

data OpsHandler ops a r eff = OpsHandler {
  handleReturn :: a -> eff r,
  handleOps :: CoOperation ops (eff r) -> eff r
}

newtype DynamicEff ops eff a = DynamicEff {
  runDynamicEff :: forall r . OpsHandler ops a r eff -> eff r
}

type DynamicHandler ops1 ops2 a r eff =
  Computation ops1 (OpsHandler ops2 a r) eff

data DynamicComputation ops comp eff where
  BindDynamic
    :: forall ops comp eff1 .
    (forall eff2 .
      (Effect eff2)
      => LiftEff eff1 eff2
      -> Operation ops eff2
      -> comp eff2)
    -> DynamicComputation ops comp eff1

class (EffOps ops) => DynamicOps ops where
  dynamicOps
    :: forall eff .
    (Effect eff)
    => Operation ops (DynamicEff ops eff)

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

liftReturn
  :: forall ops eff a .
  ( Effect eff
  , EffOps ops
  )
  => eff a
  -> DynamicEff ops eff a
liftReturn mx = DynamicEff $ \handler -> do
  x <- mx
  handleReturn handler x

liftOps
  :: forall ops eff a .
  ( Effect eff
  , EffOps ops
  )
  => CoOperation ops (eff a)
  -> DynamicEff ops eff a
liftOps ops = DynamicEff $ cont
 where
  cont :: forall r . OpsHandler ops a r eff -> eff r
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
  => OpsHandler ops a b eff
  -> (OpsConstraint ops (DynamicEff ops eff)
      => DynamicEff ops eff a)
  -> eff b
handleDynamic handler comp1 = runDynamicEff comp2 handler
 where
  ops :: Operation ops (DynamicEff ops eff)
  ops = dynamicOps

  comp2 :: DynamicEff ops eff a
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
          => OpsHandler ops2 a r eff2))
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
      => OpsHandler ops2 a r eff2)
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
  handler2 :: OpsHandler ops1 a r eff
  handler2 = runComp handler1 id

  comp2 :: eff r
  comp2 = handleDynamic handler2 $ returnVal $
    runComp comp1 liftReturn
