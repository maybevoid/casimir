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

newtype FreeMonad ops m a = FreeMonad {
  unFreeT ::FreeT (CoOperation ops) m a
}

deriving newtype
  instance (FreeOps ops, Monad m) => Functor (FreeMonad ops m)

deriving newtype
  instance (FreeOps ops, Monad m) => Applicative (FreeMonad ops m)

deriving newtype
  instance (FreeOps ops, Monad m) => Monad (FreeMonad ops m)

instance FreeEff FreeMonad where
  freeOps = freeMonadOps
  liftFree = liftFreeMonad

instance FreeHandler FreeMonad where
  handleFree = handleFreeMonad

liftFreeMonad
  :: forall m ops a
   . (Monad m, FreeOps ops)
   => m a
   -> FreeMonad ops m a
liftFreeMonad = FreeMonad . lift
{-# INLINE liftFreeMonad #-}

freeMonadOps
  :: forall ops m .
  (FreeOps ops, Monad m)
  => Operations ops (FreeMonad ops m)
freeMonadOps = mkFreeOps (FreeMonad . liftF)
{-# INLINE freeMonadOps #-}

handleFreeMonad
  :: forall ops m a r
   . (Monad m, FreeOps ops)
  => CoOpHandler ops a r m
  -> FreeMonad ops m a
  -> m r
handleFreeMonad (CoOpHandler handleReturn handleCoOp) (FreeMonad m) = handleFree' m
 where
  handleFree'
   :: FreeT (CoOperation ops) m a
    -> m r
  handleFree' comp = runFreeT comp >>= handleComp

  handleComp
    :: FreeF (CoOperation ops) a (FreeT (CoOperation ops) m a)
    -> m r
  handleComp (Pure x) = handleReturn x
  handleComp (Free ops) = handleCoOp $ fmap handleFree' ops
{-# INLINE handleFreeMonad #-}
