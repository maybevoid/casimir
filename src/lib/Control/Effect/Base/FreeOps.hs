{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Effect.Base.FreeOps
where

import Control.Natural (type (~>))
import Control.Monad.Trans.Free (FreeT)
import Control.Monad.Trans.Class (lift)

import Control.Effect.Base.Effect

type FreeEff ops eff a = FreeT (CoOperation ops) eff a

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

class FreeOps (ops :: *) where
  type family Operation ops
    = (f :: (* -> *) -> *) | f -> ops

  type family CoOperation ops
    = (f :: (* -> *)) | f -> ops

  freeOps
    :: forall ops' eff .
    (Functor ops', Effect eff)
    => (CoOperation ops ~> ops')
    -> Operation ops (FreeT ops' eff)

liftFree
  :: forall eff ops a
   . (Effect eff, FreeOps ops)
   => eff a
   -> FreeEff ops eff a
liftFree = lift
