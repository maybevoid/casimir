{-# LANGUAGE FlexibleContexts #-}

module Demo where

import Data.IORef

import Control.Monad.Identity
import Control.Monad.Reader (ReaderT (..))
import qualified Control.Monad.Reader as MR
import Control.Monad.Trans.Class (MonadTrans (..))

import Control.Effect.Util
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

refStateOps :: forall a eff .
  (IoEff eff)
  => IORef a
  -> StateOps a eff
refStateOps ref = StateOps {
  getOp = liftIO $ readIORef ref,
  putOp = liftIO . (writeIORef ref)
}

comp1 :: forall eff .
  (Effect eff, EnvEff Int eff)
  => eff Int
comp1 = do
  val <- ask
  return $ val + 1

comp2 :: EffectfulValue (EnvOps Int) Int
comp2 = effectfulValue comp1

-- comp2 :: forall eff .
--   (Effect eff)
--   => ReaderT Int eff Int
-- comp2 = handleReaderT NoOp comp1

-- readerTHandler :: forall a eff . (Effect eff) => EnvOps a (ReaderT a eff)
-- readerTHandler = EnvOps {
--   askOp = MR.ask
-- }

-- handleReaderT
--   :: forall r a eff effRow .
--   (Effect eff, EffOps effRow)
--   => (forall eff' . effRow eff')
--   -> (( EffConstraint effRow (ReaderT a eff)
--       , EnvEff a (ReaderT a eff)
--       )
--       => ReaderT a eff r)
--   -> ReaderT a eff r
-- handleReaderT effRow comp =
--   bindConstraint effRow' comp
--     where
--       effRow' :: Union (EnvOps a) effRow (ReaderT a eff)
--       effRow' = stackEffHandlers readerTHandler effRow $ LiftEff lift

-- refStateHandler :: forall a eff .
--   (IoEff eff)
--   => IORef a
--   -> StateOps a eff
-- refStateHandler ref = StateOps {
--   getOp = liftIO $ readIORef ref,
--   putOp = liftIO . (writeIORef ref)
-- }

-- ioHandler :: IoOps IO
-- ioHandler = IoOps {
--   liftIoOp = id
-- }

-- ioAndStateHandler
--   :: forall a .
--   IORef a
--   -> Union IoOps (StateOps a) IO
-- ioAndStateHandler ref =
--   composeEffHandlers ioHandler (refStateHandler ref)

-- comp3 :: ReaderT Int Identity Int
-- comp3 = comp2

-- comp4 :: Identity Int
-- comp4 = bindConstraint (mkEnvHandler 3) comp1
