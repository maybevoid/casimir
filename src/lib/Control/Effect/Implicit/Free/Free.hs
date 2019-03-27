{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Effect.Implicit.Free.Free
  ( FreeMonad (..)
  )
where

import Control.Monad.Trans.Free
import Control.Monad.Trans.Class (lift)

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Free.FreeEff

newtype FreeMonad ops eff a = FreeMonad {
  unFreeT ::FreeT (CoOperation ops) eff a
}

deriving newtype
  instance (FreeOps ops, Monad eff) => Functor (FreeMonad ops eff)

deriving newtype
  instance (FreeOps ops, Monad eff) => Applicative (FreeMonad ops eff)

deriving newtype
  instance (FreeOps ops, Monad eff) => Monad (FreeMonad ops eff)

instance FreeEff FreeMonad where
  freeOps = freeMonadOps
  liftFree = liftFreeMonad
  handleFree = handleFreeMonad

liftFreeMonad
  :: forall eff ops a
   . (Effect eff, FreeOps ops)
   => eff a
   -> FreeMonad ops eff a
liftFreeMonad = FreeMonad . lift
{-# INLINE liftFreeMonad #-}

freeMonadOps
  :: forall ops eff .
  (FreeOps ops, Effect eff)
  => Operation ops (FreeMonad ops eff)
freeMonadOps = mkFreeOps (FreeMonad . liftF)
{-# INLINE freeMonadOps #-}

handleFreeMonad
  :: forall ops eff a r
   . (Effect eff, FreeOps ops)
  => CoOpHandler ops a r eff
  -> FreeMonad ops eff a
  -> eff r
handleFreeMonad handler (FreeMonad m) = handleFree' m
 where
  handleFree'
   :: FreeT (CoOperation ops) eff a
    -> eff r
  handleFree' comp = runFreeT comp >>= handleComp

  handleComp
    :: FreeF (CoOperation ops) a (FreeT (CoOperation ops) eff a)
    -> eff r
  handleComp (Pure x) = handleReturn handler x
  handleComp (Free ops) = handleCoOp handler $ fmap handleFree' ops
{-# INLINE handleFreeMonad #-}
