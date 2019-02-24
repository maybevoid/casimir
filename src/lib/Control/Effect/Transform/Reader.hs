
module Control.Effect.Transform.Reader
  ( readerTHandler
  , runReaderTHandler
  )
where

import Control.Monad.Reader
  (ReaderT (..), ask, runReaderT)

import Control.Monad.Trans.Class
  (MonadTrans (..))

import Control.Effect.Handler (mkHandler)
import Control.Effect.Ops.NoOp (NoOp (..))
import Control.Effect.Ops.Env (EnvOps (..))

import Control.Effect.Class
  ( Effect
  , Handler
  , LiftEff (..)
  , liftEff
  )

readerTHandler
  :: forall a eff .
  (Effect eff)
  => Handler NoOp (EnvOps a) (ReaderT a eff) eff
readerTHandler = mkHandler (LiftEff lift) $
  \lifter -> EnvOps {
    askOp = liftEff lifter ask
  }

runReaderTHandler
  :: forall a eff .
  (Effect eff)
  => a
  -> Handler NoOp NoOp eff (ReaderT a eff)
runReaderTHandler x =
  mkHandler (LiftEff lifter) $ \_ -> NoOp
    where
      lifter :: forall x . ReaderT a eff x -> eff x
      lifter eff = runReaderT eff x