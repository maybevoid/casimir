{-# LANGUAGE FlexibleContexts #-}

module Demo where

import Data.IORef

import Control.Effect.Cast
import Control.Effect.Class
import Control.Effect.Union
import Control.Effect.Handler

import Control.Effect.Ops.IO
import Control.Effect.Ops.Env
import Control.Effect.Ops.NoOp
import Control.Effect.Ops.State

mkEnvOps :: forall a eff . (Effect eff) => a -> EnvOps a eff
mkEnvOps x = EnvOps {
  askOp = return x
}

mkEnvHandler
  :: forall a eff .
  (Effect eff)
  => a
  -> BaseHandler (EnvOps a) eff
mkEnvHandler = baseHandler . mkEnvOps

readerComp1 :: forall eff .
  (Effect eff, EnvEff Int eff)
  => eff Int
readerComp1 = do
  val <- ask
  return $ val + 1

readerComp2 :: Identity Int
readerComp2 = withHandler (mkEnvHandler 3) readerComp1

refStateOps
  :: forall a eff .
  (IoEff eff)
  => IORef a
  -> StateOps a eff
refStateOps ref = StateOps {
  getOp = liftIO $ readIORef ref,
  putOp = liftIO . (writeIORef ref)
}

refStateHandler :: forall a . IORef a -> GenericHandler IoOps (StateOps a)
refStateHandler ioRef = genericHandler $ refStateOps ioRef

ioOps :: IoOps IO
ioOps = IoOps {
  liftIoOp = id
}

ioHandler :: BaseHandler IoOps IO
ioHandler = baseHandler ioOps

ioAndStateHandler
  :: forall a .
  IORef a
  -> BaseHandler (Union IoOps (StateOps a)) IO
ioAndStateHandler ref = castHandler handler $ CastOps Cast
  where
    handler = composeHandlers
      @_ @_ @NoOp @NoOp
      ioHandler
      (refStateHandler ref)
      (CastOps Cast)
      (CastOps Cast)

stateIoComp1
  :: forall eff .
  (Effect eff, EffConstraint IoOps eff)
  => eff Int
stateIoComp1 = do
  ref <- liftIO $ newIORef 3
  withHandler (refStateHandler ref) $ do
    state <- get
    put $ state + 1
  finalVal <- liftIO $ readIORef ref
  return finalVal

stateIoComp4 :: IO Int
stateIoComp4 = withHandler ioHandler stateIoComp1