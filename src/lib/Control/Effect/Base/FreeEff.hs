{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Effect.Base.FreeEff
where

import Control.Effect.Base.Effect
import Control.Effect.Base.LiftEff
import Control.Effect.Base.FreeOps

class
  (forall ops eff . (FreeOps ops, Effect eff) => Monad (free ops eff))
  => FreeEff free
  where
    freeOps :: forall ops eff .
      (FreeOps ops, Effect eff)
      => Operation ops (free ops eff)

    liftFree :: forall ops eff a .
      (FreeOps ops, Effect eff)
      => eff a -> free ops eff a

    handleFree
      :: forall ops eff a r
      . (Effect eff, FreeOps ops)
      => OpsHandler ops a r eff
      -> free ops eff a
      -> eff r

data OpsHandler handler a r eff = OpsHandler {
  handleReturn :: a -> eff r,
  handleOps :: CoOperation handler (eff r) -> eff r
}

newtype GenericOpsHandler handler eff
  = GenericOpsHandler
    (forall a . OpsHandler handler a a eff)

data ContextualHandler w handler eff = ContextualHandler {
  runContextualHandler
    :: forall a . OpsHandler handler a (w eff a) eff,

  extractResult :: forall a . w eff a -> eff a
}

freeLiftEff
  :: forall ops free eff
   . (FreeEff free, FreeOps ops, Effect eff)
  => LiftEff eff (free ops eff)
freeLiftEff = mkLiftEff liftFree
