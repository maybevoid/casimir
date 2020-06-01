
module Casimir.Ops.Env.Transform
where

import Control.Monad.Reader
  (ReaderT (..), ask)

import Control.Monad.Trans.Class
  (MonadTrans (..))

import Casimir.Base
import Casimir.Ops.Env.Base
  ( EnvOps (..)
  )

liftReaderT
  :: forall e m a
   . (Monad m)
  => m a
  -> ReaderT e m a
liftReaderT = lift

readerTLift
  :: forall a m
   . (Monad m)
  => Lift m (ReaderT a m)
readerTLift = Lift lift

readerTOps :: forall a m . (Monad m)
  => EnvOps a (ReaderT a m)
readerTOps = EnvOps {
  askOp = ask
}
