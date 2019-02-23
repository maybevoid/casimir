{-# LANGUAGE FlexibleContexts #-}

module Demo where

import Data.IORef

import Control.Effect.Cast
import Control.Effect.Class
import Control.Effect.Union
import Control.Effect.Handler
import Control.Effect.Computation

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

comp1 :: forall eff .
  (Effect eff, EnvEff Int eff)
  => eff Int
comp1 = do
  val <- ask
  return $ val + 1

comp2 :: forall eff . (Effect eff) => EffectfulValue (EnvOps Int) Int eff
comp2 = effectfulValue comp1

comp3 :: forall eff . (Effect eff) => Return Int eff
comp3 = withHandler (mkEnvHandler 3) comp2 (CastOps Cast)

comp4 :: Int
comp4 = extractReturn comp3

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
