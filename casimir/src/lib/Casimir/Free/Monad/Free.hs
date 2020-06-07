{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Casimir.Free.Monad.Free
  ( FreeMonad (..)
  )
where

import Control.Monad.Trans.Free
import Control.Monad.Trans.Class (lift)

import Casimir.Base
import Casimir.Free.CoOp
import Casimir.Free.FreeOps
import Casimir.Free.FreeEff

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

instance FreeHandler FreeMonad where
  handleFree = handleFreeMonad

liftFreeMonad
  :: forall eff ops a
   . (Monad eff, FreeOps ops)
   => eff a
   -> FreeMonad ops eff a
liftFreeMonad = FreeMonad . lift
{-# INLINE liftFreeMonad #-}

freeMonadOps
  :: forall ops eff .
  (FreeOps ops, Monad eff)
  => Operation ops (FreeMonad ops eff)
freeMonadOps = mkFreeOps (FreeMonad . liftF)
{-# INLINE freeMonadOps #-}

handleFreeMonad
  :: forall ops eff a r
   . (Monad eff, FreeOps ops)
  => CoOpHandler ops a r eff
  -> FreeMonad ops eff a
  -> eff r
handleFreeMonad (CoOpHandler handleReturn handleCoOp) (FreeMonad m) = handleFree' m
 where
  handleFree'
   :: FreeT (CoOperation ops) eff a
    -> eff r
  handleFree' comp = runFreeT comp >>= handleComp

  handleComp
    :: FreeF (CoOperation ops) a (FreeT (CoOperation ops) eff a)
    -> eff r
  handleComp (Pure x) = handleReturn x
  handleComp (Free ops) = handleCoOp $ fmap handleFree' ops
{-# INLINE handleFreeMonad #-}
