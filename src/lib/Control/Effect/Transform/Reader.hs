
module Control.Effect.Transform.Reader
where

import Control.Monad.Reader
  (ReaderT (..), ask)

import Control.Monad.Trans.Class
  (MonadTrans (..))

import Control.Effect.Computation
import Control.Effect.Ops.Env (EnvEff)
import Control.Effect.Ops.LabeledEnv (LabeledEnvOps (..))

import Control.Effect.Base

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
  \lifter -> LabeledEnvOps {
    askOp = liftEff lifter ask
  }
