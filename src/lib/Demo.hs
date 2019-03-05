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
  -> BaseHandler (EnvEff a) eff
mkEnvHandler = baseHandler . mkEnvOps

envHandler1 :: forall eff . (Effect eff) => BaseHandler (EnvEff Int) eff
envHandler1 = mkEnvHandler 3

envHandler2 :: forall eff . (Effect eff) => BaseHandler (EnvEff Int) eff
envHandler2 = mkEnvHandler 8

envHandler3
  :: forall eff .
  (Effect eff)
  => BaseHandler (Union (EnvEff Int) (EnvEff Int)) eff
envHandler3 = composeHandlersWithCast
  envHandler1 envHandler2
  (opsCast cast)
  (opsCast cast)

readerComp1 :: forall eff .
  (Effect eff, OpsConstraint (EnvEff Int) eff)
  => eff Int
readerComp1 = do
  val <- ask
  return $ val + 1

readerComp2 :: Identity Int
readerComp2 = withHandler envHandler1 readerComp1

envHandler4 :: FreeHandler (EnvEff Int)
envHandler4 = freeHandler

readerComp3 :: Free (EnvModel Int) Int
readerComp3 = withHandler envHandler4 readerComp1

readerComp4 :: GenericComputation (EnvEff Int) Int
readerComp4 = genericComputation readerComp1

readerComp5 :: IdentityComputation Int
readerComp5 = bindHandlerWithCast
  envHandler1 readerComp4
  (opsCast cast)
  (opsCast cast)

readerComp6 :: Int
readerComp6 = runIdentityComp readerComp5

readerComp7 :: forall eff .
  (Effect eff)
  => EffectfulComputation
    (Union NoEff (Union (EnvEff Int) NoEff))
    Int
    eff
readerComp7 = castComputation (opsCast cast) readerComp4

readerComp8 :: IdentityComputation Int
readerComp8 = bindHandlerWithCast
  envHandler1
  readerComp7
  (opsCast cast)
  (opsCast cast)

readerComp9 :: Identity Int
readerComp9 = withHandler envHandler3 readerComp1

readerComp10 :: Identity Int
readerComp10 = withHandler envHandler1 comp
  where
    comp :: (OpsConstraint (EnvEff Int) Identity) => Identity Int
    comp = withHandler envHandler2 readerComp1

readerComp11 :: Int
readerComp11 = runIdentityComp $
  bindHandlerWithCast
    envHandler3 readerComp4
    (opsCast cast)
    (opsCast cast)

readerComp12 :: forall eff . (Effect eff) => EffectfulComputation NoEff Int eff
readerComp12 = bindHandlerWithCast
  envHandler2 readerComp4
  (opsCast cast)
  (opsCast cast)

readerComp13 :: forall eff . (Effect eff) => EffectfulComputation NoEff Int eff
readerComp13 = bindHandlerWithCast
  envHandler1 readerComp12
  (OpsCast Cast)
  (OpsCast Cast)

readerComp14 :: Int
readerComp14 = runIdentityComp readerComp13

refStateOps
  :: forall a eff .
  (IoConstraint eff)
  => IORef a
  -> StateOps a eff
refStateOps ref = StateOps {
  getOp = liftIo $ readIORef ref,
  putOp = liftIo . (writeIORef ref)
}

refStateHandler :: forall a . IORef a -> GenericHandler IoEff (StateEff a)
refStateHandler ioRef = genericHandler $ refStateOps ioRef

ioOps :: IoOps IO
ioOps = IoOps {
  liftIoOp = id
}

ioHandler :: BaseHandler IoEff IO
ioHandler = baseHandler ioOps

ioAndStateHandler
  :: forall a .
  IORef a
  -> BaseHandler (Union IoEff (StateEff a)) IO
ioAndStateHandler ref = handler
  where
    handler = composeHandlersWithCast
      ioHandler
      (refStateHandler ref)
      (opsCast cast)
      (opsCast cast)

stateIoComp1
  :: forall eff .
  (Effect eff, OpsConstraint IoEff eff)
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

trueHandler
  :: forall eff .
  (Effect eff)
  => OpsHandler (DecideEff Bool) Int String eff
trueHandler = OpsHandler {
  handleReturn = return . show,
  handleOps = \(DecideOp cont) -> cont True
}

nonDetHandler1
  :: forall eff .
  (Effect eff)
  => OpsHandler (DecideEff Bool) Int [Int] eff
nonDetHandler1 = OpsHandler {
  handleReturn = \x -> return [x],
  handleOps = \(DecideOp cont) -> do
    res1 <- cont True
    res2 <- cont False
    return $ res1 ++ res2
}

nonDetHandler2
  :: forall eff .
  (Effect eff)
  => DynamicHandler NoEff (DecideEff Bool) Int [Int] eff
nonDetHandler2 = genericDynamicHandler nonDetHandler1

decideComp1
  :: forall eff .
  ( Effect eff
  , DecideConstraint Bool eff
  , IoConstraint eff
  ) => eff Int
decideComp1 = do
  a <- decide
  liftIo $ putStrLn $ "a: " ++ (show a)
  b <- decide
  liftIo $ putStrLn $ "b: " ++ (show b)
  return $ if a
    then if b then 1 else 2
    else if b then 3 else 4

decideComp2
  :: forall eff .
  (Effect eff)
  => Computation (Union IoEff (DecideEff Bool)) (Return Int) eff
decideComp2 = genericComputation decideComp1

decideComp3 :: EffectfulComputation (DecideEff Bool) Int IO
decideComp3 = bindHandlerWithCast
  ioHandler decideComp2
  (opsCast cast)
  (opsCast cast)

decideComp4 :: IO [Int]
decideComp4 = applyDynamic nonDetHandler2 decideComp3
