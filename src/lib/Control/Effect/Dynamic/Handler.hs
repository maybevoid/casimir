
module Control.Effect.Dynamic.Handler
where

import Control.Monad.Identity

import Control.Effect.Base
import Control.Effect.Dynamic.Class

withOpsHandler
  :: forall ops eff a b .
  ( Effect eff
  , EffOps ops
  , DynamicOps ops
  )
  => OpsHandler ops a b eff
  -> (OpsConstraint ops (DynamicEff ops eff)
      => DynamicEff ops eff a)
  -> eff b
withOpsHandler handler comp1 = runDynamicEff comp2 handler
 where
  ops :: Operation ops (DynamicEff ops eff)
  ops = dynamicOps

  comp2 :: DynamicEff ops eff a
  comp2 = bindConstraint ops comp1

identityOpsHandler
  :: forall handler eff .
  (Effect eff, EffOps handler)
  => (forall a . OpsHandler handler a a eff)
  -> GenericOpsHandler Identity handler eff
identityOpsHandler handler1 = GenericOpsHandler handler2 runIdentity
 where
  handler2 :: forall a . OpsHandler handler a (Identity a) eff
  handler2 = OpsHandler {
    handleReturn = wrapReturn,
    handleOps = wrapOps
  }
   where
    wrapReturn :: a -> eff (Identity a)
    wrapReturn x = fmap Identity $ handleReturn handler1 x

    wrapOps :: CoOperation handler (eff (Identity a)) -> eff (Identity a)
    wrapOps ops = fmap Identity $ handleOps handler1 $
      (fmap . fmap) runIdentity ops