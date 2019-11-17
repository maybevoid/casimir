{-# LANGUAGE FlexibleContexts #-}

module Demo where

import Data.IORef
import Control.Monad.Identity

import Control.Effect.Implicit
import Control.Effect.Implicit.Free
import Control.Effect.Implicit.Ops

envHandler1 :: forall eff . (Effect eff)
  => BaseOpsHandler NoEff (EnvEff Int) eff
envHandler1 = mkEnvHandler 3

envHandler2 :: forall eff . (Effect eff)
  => BaseOpsHandler NoEff (EnvEff Int) eff
envHandler2 = mkEnvHandler 8

envHandler3 :: forall eff . (Effect eff)
  => BaseOpsHandler NoEff (Union (EnvEff Int) (EnvEff Int)) eff
envHandler3 = composeOpsHandlers
  envHandler1 envHandler2

readerComp1 :: Eff (EnvEff Int) Int
readerComp1 = do
  val <- ask
  return $ val + 1

readerComp2 :: Identity Int
readerComp2 = withOpsHandler envHandler1 readerComp1

envHandler4
  :: forall free eff
   . (Effect eff, FreeEff free)
  => BaseOpsHandler NoEff (EnvEff Int) (free (EnvEff Int) eff)
envHandler4 = baseOpsHandler $ freeOps @free

readerComp3 :: FreeMonad (EnvEff Int) Identity Int
readerComp3 = withOpsHandler envHandler4 readerComp1

readerComp4 :: forall eff . (Effect eff)
  => BaseComputation (EnvEff Int) (Return Int) eff
readerComp4 = genericReturn readerComp1

readerComp5 :: BaseComputation NoEff (Return Int) Identity
readerComp5 = bindOpsHandler
  envHandler1 readerComp4

readerComp6 :: Int
readerComp6 = runIdentityComp readerComp5

readerComp7 :: forall eff .
  (Effect eff)
  => BaseComputation
    (Union NoEff (Union (EnvEff Int) NoEff))
    (Return Int)
    eff
readerComp7 = castComputation cast readerComp4

readerComp8 :: BaseComputation NoEff (Return Int) Identity
readerComp8 = bindOpsHandler
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
  bindOpsHandler envHandler3 readerComp4

readerComp12 :: forall eff . (Effect eff)
  => BaseComputation NoEff (Return Int) eff
readerComp12 = bindOpsHandler envHandler2 readerComp4

readerComp13 :: forall eff . (Effect eff)
  => BaseComputation NoEff (Return Int) eff
readerComp13 = bindOpsHandler envHandler1 readerComp12

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

refStateHandler :: forall  eff a . (Effect eff)
  => IORef a
  -> BaseOpsHandler IoEff (StateEff a) eff
refStateHandler ioRef = genericOpsHandler $ refStateOps ioRef

refStatePipeline
  :: forall eff a .
  (Effect eff)
  => IORef a
  -> GenericPipeline LiftEff IoEff (StateEff a) eff
refStatePipeline ref = opsHandlerToPipeline $ refStateHandler ref

ioPipeline
  :: GenericPipeline LiftEff NoEff IoEff IO
ioPipeline = opsHandlerToPipeline ioHandler

ioAndStateHandler
  :: forall a .
  IORef a
  -> BaseOpsHandler NoEff (IoEff ∪ StateEff a) IO
ioAndStateHandler ref = handler
  where
    handler = composeOpsHandlers
      ioHandler
      (refStateHandler ref)

stateIoPipeline
  :: forall a .
  IORef a
  -> GenericPipeline LiftEff NoEff (Union IoEff (StateEff a)) IO
stateIoPipeline ref = composePipelines
  (refStatePipeline ref)
  ioPipeline

stateIoComp1 :: Eff IoEff Int
stateIoComp1 = do
  ref <- liftIo $ newIORef 3
  withOpsHandler (refStateHandler ref) $ do
    state <- get
    put $ state + 1
  liftIo $ readIORef ref

stateIoComp2 :: IO Int
stateIoComp2 = withOpsHandler ioHandler stateIoComp1

stateComp1 :: Eff (StateEff Int) Int
stateComp1 = do
  state1 <- get
  put $ state1 + 1
  state2 <- get
  return $ state2 + 1

stateComp2 :: forall eff . (Effect eff)
  => BaseComputation (StateEff Int) (Return Int) eff
stateComp2 = genericReturn stateComp1

stateIoComp3 :: IORef Int -> BaseComputation NoEff (Return Int) IO
stateIoComp3 ref = runPipeline
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
  => BaseComputation NoEff (CoOpHandler (DecideEff Bool) Int [Int]) eff
nonDetHandler2 = Computation $ \ _ _ -> nonDetHandler1

nonDetPipeline
  :: forall eff .
  (Effect eff)
  => BasePipeline NoEff (DecideEff Bool) (Return Int) (Return [Int]) eff eff
nonDetPipeline = coopHandlerToPipeline @ChurchMonad nonDetHandler2

decideComp1 :: Eff (IoEff ∪ DecideEff Bool) Int
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
  => BaseComputation (IoEff ∪ (DecideEff Bool)) (Return Int) eff
decideComp2 = genericReturn decideComp1

decideComp3 :: BaseComputation (DecideEff Bool) (Return Int) IO
decideComp3 = bindOpsHandler
  ioHandler decideComp2

decideComp4 :: IO [Int]
decideComp4 =
  withCoOpHandler @ChurchMonad nonDetHandler1 $
    returnVal $ runComp decideComp3 freeLiftEff captureOps

decideComp5
  :: forall eff .
  (Effect eff)
  => BaseComputation IoEff (Return [Int]) eff
decideComp5 = runPipeline
  nonDetPipeline decideComp2

decideComp6 :: BaseComputation NoEff (Return [Int]) IO
decideComp6 = runPipeline
  ioPipeline decideComp5

decideComp7 :: IO [Int]
decideComp7 = returnVal $ runComp decideComp6 idLift NoOp

pipeline1
  :: BasePipeline
      NoEff
      (Union (DecideEff Bool) IoEff)
      (Return Int)
      (Return [Int])
      IO
      IO
pipeline1 = composePipelines
  ioPipeline nonDetPipeline

decideComp8 :: IO [Int]
decideComp8 = returnVal $ runComp comp idLift NoOp
 where
  comp :: BaseComputation NoEff (Return [Int]) IO
  comp = runPipeline
    pipeline1 decideComp2

ops1 :: Operation ((EnvEff Int) ∪ IoEff) IO
ops1 = UnionOps (mkEnvOps 2) ioOps

ops2 :: Operation (IoEff ∪ (EnvEff Int)) IO
ops2 = UnionOps ioOps (mkEnvOps 3)

ops3
  :: Operation
      ( ((EnvEff Int) ∪ IoEff)
      ∪ (IoEff ∪ (EnvEff Int))
      )
      IO
ops3 = UnionOps ops1 ops2
