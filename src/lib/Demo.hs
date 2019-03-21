{-# LANGUAGE FlexibleContexts #-}

module Demo where

import Data.IORef
import Control.Monad.Identity

import Control.Effect

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
  cast
  cast

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

readerComp3 :: FreeMonad (EnvEff Int) Identity Int
readerComp3 = withHandler envHandler4 readerComp1

readerComp4 :: GenericReturn (EnvEff Int) Int
readerComp4 = genericComputation $ Return readerComp1

readerComp5 :: IdentityComputation Int
readerComp5 = bindHandlerWithCast
  envHandler1 readerComp4
  cast
  cast

readerComp6 :: Int
readerComp6 = runIdentityComp readerComp5

readerComp7 :: forall eff .
  (Effect eff)
  => ReturnComputation
    (Union NoEff (Union (EnvEff Int) NoEff))
    Int
    eff
readerComp7 = castComputation cast readerComp4

readerComp8 :: IdentityComputation Int
readerComp8 = bindHandlerWithCast
  envHandler1
  readerComp7
  cast
  cast

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
    cast
    cast

readerComp12 :: forall eff . (Effect eff) => ReturnComputation NoEff Int eff
readerComp12 = bindHandlerWithCast
  envHandler2 readerComp4
  cast
  cast

readerComp13 :: forall eff . (Effect eff) => ReturnComputation NoEff Int eff
readerComp13 = bindHandlerWithCast
  envHandler1 readerComp12
  cast
  cast

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

refStatePipeline
  :: forall eff a .
  (Effect eff)
  => IORef a
  -> GenericPipeline IoEff (StateEff a) eff
refStatePipeline ref = handlerToPipeline $ refStateHandler ref

ioPipeline
  :: GenericPipeline NoEff IoEff IO
ioPipeline = handlerToPipeline ioHandler

ioAndStateHandler
  :: forall a .
  IORef a
  -> BaseHandler (Union IoEff (StateEff a)) IO
ioAndStateHandler ref = handler
  where
    handler = composeHandlersWithCast
      ioHandler
      (refStateHandler ref)
      cast
      cast

stateIoPipeline
  :: forall a .
  IORef a
  -> GenericPipeline NoEff (Union IoEff (StateEff a)) IO
stateIoPipeline ref = composePipelinesWithCast
  (refStatePipeline ref)
  ioPipeline
  cast
  cast
  cast

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

stateComp1
  :: forall eff .
  (Effect eff, OpsConstraint (StateEff Int) eff)
  => eff Int
stateComp1 = do
  state1 <- get
  put $ state1 + 1
  state2 <- get
  return $ state2 + 1

stateComp2 :: GenericReturn (StateEff Int) Int
stateComp2 = genericComputation $ Return stateComp1

stateIoComp3 :: IORef Int -> Computation NoEff (Return Int) IO
stateIoComp3 ref = runPipelineWithCast
  (stateIoPipeline ref)
  stateComp2
  cast
  cast

stateIoComp4 :: IO Int
stateIoComp4 = do
  ref <- newIORef 3
  execComp $ stateIoComp3 ref

nonDetHandler1
  :: forall eff .
  (Effect eff)
  => CoOpHandler (DecideEff Bool) Int [Int] eff
nonDetHandler1 = CoOpHandler {
  handleReturn = \x -> return [x],
  handleCoOp = \(DecideOp x y cont) -> do
    res1 <- cont x
    res2 <- cont y
    return $ res1 ++ res2
}

nonDetHandler2
  :: forall eff .
  (Effect eff)
  => Computation NoEff (CoOpHandler (DecideEff Bool) Int [Int]) eff
nonDetHandler2 = Computation $ \ _ _ -> nonDetHandler1

nonDetPipeline
  :: forall eff .
  (Effect eff)
  => Pipeline NoEff (DecideEff Bool) eff eff (Return Int) (Return [Int])
nonDetPipeline = coopHandlerToPipeline @ChurchMonad nonDetHandler2

decideComp1
  :: forall eff .
  ( Effect eff
  , DecideConstraint Bool eff
  , IoConstraint eff
  ) => eff Int
decideComp1 = do
  a <- decide True False
  liftIo $ putStrLn $ "a: " ++ (show a)
  b <- decide False True
  liftIo $ putStrLn $ "b: " ++ (show b)
  return $ if a
    then if b then 1 else 2
    else if b then 3 else 4

decideComp2
  :: forall eff .
  (Effect eff)
  => Computation (Union IoEff (DecideEff Bool)) (Return Int) eff
decideComp2 = genericComputation $ Return decideComp1

decideComp3 :: ReturnComputation (DecideEff Bool) Int IO
decideComp3 = bindHandlerWithCast
  ioHandler decideComp2
  cast
  cast

decideComp4 :: IO [Int]
decideComp4 =
  withCoOpHandler @ChurchMonad nonDetHandler1 $
    returnVal $ runComp decideComp3 churchLiftEff captureOps

decideComp5
  :: forall eff .
  (Effect eff)
  => Computation IoEff (Return [Int]) eff
decideComp5 = runPipelineWithCast
  nonDetPipeline decideComp2
  cast
  cast

decideComp6 :: Computation NoEff (Return [Int]) IO
decideComp6 = runPipelineWithCast
  ioPipeline decideComp5
  cast
  cast

decideComp7 :: IO [Int]
decideComp7 = returnVal $ runComp decideComp6 idLift NoOp

pipeline1
  :: Pipeline
      NoEff
      (Union (DecideEff Bool) IoEff)
      IO
      IO
      (Return Int)
      (Return [Int])
pipeline1 = composePipelinesWithCast
  ioPipeline nonDetPipeline
  cast
  cast
  cast

decideComp8 :: IO [Int]
decideComp8 = returnVal $ runComp comp idLift NoOp
 where
  comp :: Computation NoEff (Return [Int]) IO
  comp = runPipelineWithCast
    pipeline1 decideComp2
    cast
    cast

ops1 :: UnionOps (EnvOps Int) IoOps IO
ops1 = UnionOps (mkEnvOps 2) ioOps

ops2 :: UnionOps IoOps (EnvOps Int) IO
ops2 = UnionOps ioOps (mkEnvOps 3)

ops3 :: UnionOps
  (UnionOps (EnvOps Int) IoOps)
  (UnionOps IoOps (EnvOps Int))
  IO
ops3 = UnionOps ops1 ops2

ops4 :: UnionOps (EnvOps Int)
  (UnionOps IoOps
    (UnionOps IoOps
      (UnionOps (EnvOps Int) NoOp)))
  IO
ops4 = normalizeOps ops3

readComp1 :: forall eff
   . (Effect eff, EnvConstraint Int eff, IoConstraint eff)
   => eff Int
readComp1 = ask

readComp2
  :: forall eff
   . (Effect eff, IoConstraint eff, EnvConstraint Int eff)
   => eff Int
readComp2 = ask

readComp3 :: IO Int
readComp3 = withOps ops2 readComp1

readComp4 :: IO Int
readComp4 = withOps ops3 readComp2

readComp5 :: IO Int
readComp5 = withOps ops4 readComp2

readComp6 :: GenericReturn (Union (EnvEff Int) IoEff) Int
readComp6 = genericComputation $ Return readComp1

readComp7 :: IO Int
readComp7 = returnVal $ runComp readComp6 idLift ops1

readComp9 :: IO Int
readComp8 = returnVal $ runComp readComp6 idLift (castOps cast ops3)

readComp8 :: IO Int
readComp9 = returnVal $ runComp readComp6 idLift (castOps cast ops4)

readComp10 :: GenericReturn (Union IoEff (EnvEff Int)) Int
readComp10 = genericComputation $ Return readComp2

readComp11 :: IO Int
readComp11 = returnVal $ runComp readComp10 idLift (castOps cast ops3)

readComp12 :: IO Int
readComp12 = returnVal $ runComp readComp10 idLift (castOps cast ops4)
