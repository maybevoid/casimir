{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Effect.Base.FreeEff
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Free (FreeT, liftF)

import Control.Effect.Base.Effect
import Control.Effect.Base.FreeOps

newtype FreeMonad ops eff a = FreeMonad {
  unFreeT ::FreeT (CoOperation ops) eff a
}

deriving instance (FreeOps ops, Monad eff) => Functor (FreeMonad ops eff)
deriving instance (FreeOps ops, Monad eff) => Applicative (FreeMonad ops eff)
deriving instance (FreeOps ops, Monad eff) => Monad (FreeMonad ops eff)

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

liftFree
  :: forall eff ops a
   . (Effect eff, FreeOps ops)
   => eff a
   -> FreeMonad ops eff a
liftFree = FreeMonad . lift

freeOps
  :: forall ops eff .
  (FreeOps ops, Effect eff)
  => Operation ops (FreeMonad ops eff)
freeOps = mkFreeOps (FreeMonad . liftF)
