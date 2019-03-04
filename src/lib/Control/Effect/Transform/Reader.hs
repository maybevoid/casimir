
module Control.Effect.Transform.Reader
  ( readerTHandler
  , runReaderTHandler
  )
where

import Control.Monad.Reader
  (ReaderT (..), ask, runReaderT)

import Control.Monad.Trans.Class
  (MonadTrans (..))

import Control.Effect.Old.Handler (mkHandler)
import Control.Effect.Ops.Env (EnvEff, EnvOps (..))

import Control.Effect.Base
  ( Effect
  , NoEff
  , NoOp (..)
  )

import Control.Effect.Old.Computation

readerTHandler
  :: forall a eff .
  (Effect eff)
  => Handler NoEff (EnvEff a) (ReaderT a eff) eff
readerTHandler = mkHandler lift $
  \liftEff -> EnvOps {
    askOp = liftEff ask
  }

runReaderTHandler
  :: forall a eff .
  (Effect eff)
  => a
  -> Handler NoEff NoEff eff (ReaderT a eff)
runReaderTHandler x =
  mkHandler  lifter $ \_ -> NoOp
    where
      lifter :: forall x . ReaderT a eff x -> eff x
      lifter eff = runReaderT eff x