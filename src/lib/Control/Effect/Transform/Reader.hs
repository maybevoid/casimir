
module Control.Effect.Transform.Reader where

import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.Trans.Class (MonadTrans (..))

import Control.Effect.Ops.NoOp (NoOp)
import Control.Effect.Handler (mkHandler)
import Control.Effect.Ops.Env (EnvOps (..))
import Control.Effect.Class (Effect, Handler, LiftEff (..), liftEff)

readerTHandler :: forall a eff .
  (Effect eff)
  => Handler NoOp (EnvOps a) (ReaderT a eff) eff
readerTHandler = mkHandler (LiftEff lift) $
  \lifter -> EnvOps {
    askOp = liftEff lifter ask
  }
