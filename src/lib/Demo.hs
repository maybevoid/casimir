{-# LANGUAGE FlexibleContexts #-}

module Demo where

import Data.IORef

import Control.Monad.Trans.Free
import Control.Monad.Identity

import Control.Effect

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

envHandler1 :: forall eff . (Effect eff) => BaseHandler (EnvOps Int) eff
envHandler1 = mkEnvHandler 3

envHandler2 :: forall eff . (Effect eff) => BaseHandler (EnvOps Int) eff
envHandler2 = mkEnvHandler 8

envHandler3
  :: forall eff .
  (Effect eff)
  => BaseHandler (Union (EnvOps Int) (EnvOps Int)) eff
envHandler3 =
  composeHandlersWithCast
    @NoOp @NoOp @NoOp @NoOp
    envHandler1 envHandler2
    (CastOps Cast) (CastOps Cast)

readerComp1 :: forall eff .
  (Effect eff, EffConstraint (EnvOps Int) eff)
  => eff Int
readerComp1 = do
  val <- ask
  return $ val + 1

readerComp2 :: Identity Int
readerComp2 = withHandler envHandler1 readerComp1

envHandler4 :: FreeHandler (EnvOps Int)
envHandler4 = freeHandler

readerComp3 :: Free (EnvModel Int) Int
readerComp3 = withHandler envHandler4 readerComp1

readerComp4 :: forall eff . EffectfulComputation (EnvOps Int) Int eff
readerComp4 = effectfulComputation readerComp1

readerComp5 :: IdentityComputation Int
readerComp5 = bindHandler envHandler1 readerComp4

readerComp6 :: Int
readerComp6 = runIdentityComp readerComp5

readerComp7 :: forall eff .
  (Monad eff)
  => EffectfulComputation
    (Union NoOp (Union (EnvOps Int) NoOp))
    Int
    eff
readerComp7 = castComputation readerComp4 $ CastOps Cast

readerComp8 :: IdentityComputation Int
readerComp8 = bindHandlerWithCast envHandler1 readerComp7 (CastOps Cast)

readerComp9 :: Identity Int
readerComp9 = withHandler envHandler3 readerComp1

readerComp10 :: Identity Int
readerComp10 = withHandler envHandler1 comp
  where
    comp :: (EffConstraint (EnvOps Int) Identity) => Identity Int
    comp = withHandler envHandler2 readerComp1

readerComp11 :: Int
readerComp11 = runIdentityComp $
  bindHandlerWithCast envHandler3 readerComp4 (CastOps Cast)

readerComp12 :: forall eff . (Effect eff) => EffectfulComputation NoOp Int eff
readerComp12 = bindHandler envHandler2 readerComp4

readerComp13 :: forall eff . (Effect eff) => EffectfulComputation NoOp Int eff
readerComp13 = bindHandlerWithCast envHandler1 readerComp12 (CastOps Cast)

readerComp14 :: Int
readerComp14 = runIdentityComp readerComp13

refStateOps
  :: forall a eff .
  (IoEff eff)
  => IORef a
  -> StateOps a eff
refStateOps ref = StateOps {
  getOp = liftIo $ readIORef ref,
  putOp = liftIo . (writeIORef ref)
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
ioAndStateHandler ref = handler
  where
    handler = composeHandlers
      @NoOp @IoOps @NoOp @IoOps
      ioHandler
      (refStateHandler ref)

stateIoComp1
  :: forall eff .
  (Effect eff, EffConstraint IoOps eff)
  => eff Int
stateIoComp1 = do
  ref <- liftIo $ newIORef 3
  withHandler (refStateHandler ref) $ do
    state <- get
    put $ state + 1
  finalVal <- liftIo $ readIORef ref
  return finalVal

stateIoComp2 :: IO Int
stateIoComp2 = withHandler ioHandler stateIoComp1