
module Control.Effect.Dynamic.Handler
where

import Control.Effect.Base
import Control.Effect.Dynamic.Class

withDynamicOpsHandler
  :: forall ops eff a b .
  ( Effect eff
  , EffOps ops
  )
  => OpsHandler ops a b eff
  -> (OpsConstraint ops (DynamicMonad ops eff)
      => DynamicMonad ops eff a)
  -> eff b
withDynamicOpsHandler handler comp1 = runDynamicMonad comp2 handler
 where
  ops :: Operation ops (DynamicMonad ops eff)
  ops = dynamicOps

  comp2 :: DynamicMonad ops eff a
  comp2 = bindConstraint ops comp1
