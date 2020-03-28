
module Control.Effect.Implicit.Ops.Env.Transform
where

import Control.Monad.Reader
  (ReaderT (..), ask)

import Control.Monad.Trans.Class
  (MonadTrans (..))

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Ops.Env.Base
  ( EnvOps (..)
  )

liftReaderT
  :: forall e eff a
   . (Effect eff)
  => eff a
  -> ReaderT e eff a
liftReaderT = lift

readerTLiftEff
  :: forall a eff
   . (Effect eff)
  => LiftEff eff (ReaderT a eff)
readerTLiftEff = mkLiftEff lift

readerTOps :: forall a eff . (Effect eff)
  => EnvOps a (ReaderT a eff)
readerTOps = EnvOps {
  askOp = ask
}
