
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
  :: forall e eff a
   . (Monad eff)
  => eff a
  -> ReaderT e eff a
liftReaderT = lift

readerTLift
  :: forall a eff
   . (Monad eff)
  => Lift eff (ReaderT a eff)
readerTLift = Lift lift

readerTOps :: forall a eff . (Monad eff)
  => EnvOps a (ReaderT a eff)
readerTOps = EnvOps {
  askOp = ask
}
