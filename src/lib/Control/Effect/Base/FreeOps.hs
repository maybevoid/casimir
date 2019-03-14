{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Effect.Base.FreeOps
where

import Control.Monad.Trans.Free (FreeT, liftF)
import Control.Monad.Trans.Class (lift)

import Control.Effect.Base.Effect
import Control.Effect.Base.EffFunctor

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

class
  ( Functor (CoOperation ops)
  , EffFunctor (Operation ops)
  )
  => FreeOps (ops :: *) where
    type family Operation ops
      = (f :: (* -> *) -> *) | f -> ops

    type family CoOperation ops
      = (f :: (* -> *)) | f -> ops

    mkFreeOps
      :: forall t eff
      . ( Effect eff
        , Effect (t eff)
        )
      => (forall a . CoOperation ops a -> t eff a)
      -> Operation ops (t eff)

liftFree
  :: forall eff ops a
   . (Effect eff, FreeOps ops)
   => eff a
   -> FreeEff ops eff a
liftFree = lift

freeOps
  :: forall ops eff .
  (FreeOps ops, Effect eff)
  => Operation ops (FreeT (CoOperation ops) eff)
freeOps = mkFreeOps liftF
