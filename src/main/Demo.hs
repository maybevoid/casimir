{-# LANGUAGE FlexibleContexts #-}

module Demo where

import Data.IORef
import Control.Monad.Identity

import Control.Effect.Implicit
import Control.Effect.Implicit.Ops

envHandler1 :: forall eff . (Effect eff) => BaseOpsHandler (EnvEff Int) eff
envHandler1 = mkEnvHandler 3

envHandler2 :: forall eff . (Effect eff) => BaseOpsHandler (EnvEff Int) eff
envHandler2 = mkEnvHandler 8

envHandler3
  :: forall eff .
  (Effect eff)
  => BaseOpsHandler (Union (EnvEff Int) (EnvEff Int)) eff
envHandler3 = composeOpsHandlersWithCast
  cast cast
  envHandler1 envHandler2

readerComp1 :: forall eff .
  (EffConstraint (EnvEff Int) eff)
  => eff Int
readerComp1 = do
  val <- ask
  return $ val + 1

readerComp2 :: Identity Int
readerComp2 = withOpsHandler envHandler1 readerComp1

envHandler4
  :: forall free eff
   . (Effect eff, FreeEff free)
  => BaseOpsHandler (EnvEff Int) (free (EnvEff Int) eff)
envHandler4 = baseOpsHandler $ freeOps @free

readerComp3 :: FreeMonad (EnvEff Int) Identity Int
readerComp3 = withOpsHandler envHandler4 readerComp1

readerComp4 :: GenericReturn (EnvEff Int) Int
readerComp4 = genericReturn readerComp1

readerComp5 :: IdentityComputation Int
readerComp5 = bindOpsHandlerWithCast
  cast cast
  envHandler1 readerComp4

readerComp6 :: Int
readerComp6 = runIdentityComp readerComp5

readerComp7 :: forall eff .
  (Effect eff)
  => Computation
    (Union NoEff (Union (EnvEff Int) NoEff))
    (Return Int)
    eff
readerComp7 = castComputation cast readerComp4

readerComp8 :: IdentityComputation Int
readerComp8 = bindOpsHandlerWithCast
  cast cast
  envHandler1
  readerComp7

readerComp9 :: Identity Int
readerComp9 = withOpsHandler envHandler3 readerComp1

readerComp10 :: Identity Int
readerComp10 = withOpsHandler envHandler1 comp
  where
    comp :: (OpsConstraint (EnvEff Int) Identity) => Identity Int
    comp = withOpsHandler envHandler2 readerComp1

readerComp11 :: Int
readerComp11 = runIdentityComp $
  bindOpsHandlerWithCast
    cast cast
    envHandler3 readerComp4

readerComp12 :: forall eff . (Effect eff) => Computation NoEff (Return Int) eff
readerComp12 = bindOpsHandlerWithCast
  cast cast
  envHandler2 readerComp4

readerComp13 :: forall eff . (Effect eff) => Computation NoEff (Return Int) eff
readerComp13 = bindOpsHandlerWithCast
  cast cast
  envHandler1 readerComp12

readerComp14 :: Int
readerComp14 = runIdentityComp readerComp13

refStateOps
  :: forall a eff .
  (EffConstraint IoEff eff)
  => IORef a
  -> StateOps a eff
refStateOps ref = StateOps {
  getOp = liftIo $ readIORef ref,
  putOp = liftIo . writeIORef ref
}

refStateHandler :: forall a . IORef a -> GenericOpsHandler IoEff (StateEff a)
refStateHandler ioRef = genericOpsHandler $ refStateOps ioRef

refStatePipeline
  :: forall eff a .
  (Effect eff)
  => IORef a
  -> GenericPipeline IoEff (StateEff a) eff
refStatePipeline ref = opsHandlerToPipeline $ refStateHandler ref

ioPipeline
  :: GenericPipeline NoEff IoEff IO
ioPipeline = opsHandlerToPipeline ioHandler

ioAndStateHandler
  :: forall a .
  IORef a
  -> BaseOpsHandler (Union IoEff (StateEff a)) IO
ioAndStateHandler ref = handler
  where
    handler = composeOpsHandlersWithCast
      cast cast
      ioHandler
      (refStateHandler ref)

stateIoPipeline
  :: forall a .
  IORef a
  -> GenericPipeline NoEff (Union IoEff (StateEff a)) IO
stateIoPipeline ref = composePipelinesWithCast
  cast cast cast
  (refStatePipeline ref)
  ioPipeline

stateIoComp1
  :: forall eff .
  (EffConstraint IoEff eff)
  => eff Int
stateIoComp1 = do
  ref <- liftIo $ newIORef 3
  withOpsHandler (refStateHandler ref) $ do
    state <- get
    put $ state + 1
  liftIo $ readIORef ref

stateIoComp2 :: IO Int
stateIoComp2 = withOpsHandler ioHandler stateIoComp1

stateComp1
  :: forall eff .
  (EffConstraint (StateEff Int) eff)
  => eff Int
stateComp1 = do
  state1 <- get
  put $ state1 + 1
  state2 <- get
  return $ state2 + 1

stateComp2 :: GenericReturn (StateEff Int) Int
stateComp2 = genericReturn stateComp1

stateIoComp3 :: IORef Int -> Computation NoEff (Return Int) IO
stateIoComp3 ref = runPipelineWithCast
  cast cast
  (stateIoPipeline ref)
  stateComp2

stateIoComp4 :: IO Int
stateIoComp4 = do
  ref <- newIORef 3
  execComp $ stateIoComp3 ref

nonDetHandler1
  :: forall eff .
  (Effect eff)
  => CoOpHandler (DecideEff Bool) Int [Int] eff
nonDetHandler1 = CoOpHandler handleReturn handleCoOp
 where
  handleReturn x = return [x]
  handleCoOp (DecideOp cont) = do
    res1 <- cont True
    res2 <- cont False
    return $ res1 ++ res2

nonDetHandler2
  :: forall eff .
  (Effect eff)
  => Computation NoEff (CoOpHandler (DecideEff Bool) Int [Int]) eff
nonDetHandler2 = Computation $ \ _ _ -> nonDetHandler1

nonDetPipeline
  :: forall eff .
  (Effect eff)
  => Pipeline NoEff (DecideEff Bool) (Return Int) (Return [Int]) eff eff
nonDetPipeline = coopHandlerToPipeline @ChurchMonad nonDetHandler2

decideComp1
  :: forall eff
   . (EffConstraint (IoEff ∪ DecideEff Bool) eff)
  => eff Int
decideComp1 = do
  a <- decide
  liftIo $ putStrLn $ "a: " ++ show a
  b <- decide
  liftIo $ putStrLn $ "b: " ++ show b
  return $ if a
    then if b then 1 else 2
    else if b then 3 else 4

decideComp2
  :: forall eff .
  (Effect eff)
  => Computation (Union IoEff (DecideEff Bool)) (Return Int) eff
decideComp2 = genericReturn decideComp1

decideComp3 :: Computation (DecideEff Bool) (Return Int) IO
decideComp3 = bindOpsHandlerWithCast
  cast cast
  ioHandler decideComp2

decideComp4 :: IO [Int]
decideComp4 =
  withCoOpHandler @ChurchMonad nonDetHandler1 $
    returnVal $ runComp decideComp3 freeLiftEff captureOps

decideComp5
  :: forall eff .
  (Effect eff)
  => Computation IoEff (Return [Int]) eff
decideComp5 = runPipelineWithCast
  cast cast
  nonDetPipeline decideComp2

decideComp6 :: Computation NoEff (Return [Int]) IO
decideComp6 = runPipelineWithCast
  cast cast
  ioPipeline decideComp5

decideComp7 :: IO [Int]
decideComp7 = returnVal $ runComp decideComp6 idLift NoOp

pipeline1
  :: Pipeline
      NoEff
      (Union (DecideEff Bool) IoEff)
      (Return Int)
      (Return [Int])
      IO
      IO
pipeline1 = composePipelinesWithCast
  cast cast cast
  ioPipeline nonDetPipeline

decideComp8 :: IO [Int]
decideComp8 = returnVal $ runComp comp idLift NoOp
 where
  comp :: Computation NoEff (Return [Int]) IO
  comp = runPipelineWithCast
    cast cast
    pipeline1 decideComp2

ops1 :: UnionOps (EnvOps Int) IoOps IO
ops1 = UnionOps (mkEnvOps 2) ioOps

ops2 :: UnionOps IoOps (EnvOps Int) IO
ops2 = UnionOps ioOps (mkEnvOps 3)

ops3 :: UnionOps
  (UnionOps (EnvOps Int) IoOps)
  (UnionOps IoOps (EnvOps Int))
  IO
ops3 = UnionOps ops1 ops2
