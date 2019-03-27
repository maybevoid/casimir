
module Control.Effect.Implicit.Transform.Reader
where

import Control.Monad.Reader
  (ReaderT (..), ask)

import Control.Monad.Trans.Class
  (MonadTrans (..))

import Control.Effect.Implicit.Computation
import Control.Effect.Implicit.Ops.Env (EnvEff, EnvOps (..))

import Control.Effect.Implicit.Base

liftReaderT
  :: forall a eff
   . (Effect eff)
  => LiftEff eff (ReaderT a eff)
liftReaderT = mkLiftEff lift

readerTHandler
  :: forall a eff .
  (Effect eff)
  => Handler NoEff (EnvEff a) (ReaderT a eff) eff
readerTHandler = mkHandler liftReaderT $
  \lifter -> EnvOps {
    askOp = liftEff lifter ask
  }
