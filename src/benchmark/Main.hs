{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where
import Criterion.Main

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import Control.Effect.Implicit

import Benchmark.State

rounds :: Int
rounds = 5000

applyCurriedComp
  :: (Int -> Computation NoEff (Return a) Identity)
  -> a
applyCurriedComp comp = runIdentity $ returnVal $
  runComp (comp rounds) idLift NoOp

runReaderTComp
  :: ReaderT Int Identity ()
  -> ()
runReaderTComp m = runIdentity $ runReaderT m rounds

evalStateTComp
  :: StateT Int Identity ()
  -> ()
evalStateTComp m = runIdentity $ evalStateT m rounds

main :: IO ()
main = defaultMain [
  bgroup "State Benchmark"
    [ bench "MTL Baseline" $
        whnf evalStateTComp
        stateMTLFunc

    , bench "With StateT Ops" $
        whnf evalStateTComp
        withStateOpsComp

    , bench "With StateT Handler" $
        whnf evalStateTComp
        withStateTHandlerComp

    , bench "With StateT ReaderT Computation" $
        whnf runReaderTComp
        withStateTReaderTComp

    , bench "Bind StateT Handler Computation"  $
        whnf evalStateTComp
        stateTHandlerComp

    , bench "with CoOp Handler on ChurchMonad"  $
        whnf (\comp -> runIdentity $ comp rounds)
        (handleFreeComp @ChurchMonad)
    , bench "with CoOp Handler on FreeMonad"  $
        whnf (\comp -> runIdentity $ comp rounds)
        (handleFreeComp @FreeMonad)
    , bench "with CoOp Handler on FreerMonad"  $
        whnf (\comp -> runIdentity $ comp rounds)
        (handleFreeComp @FreerMonad)

    , bench "with Freer CoOp Handler on FreerMonad"  $
        whnf (\comp -> runIdentity $ comp rounds)
        (handleFreerComp @FreerMonad)

    , bench "Transformer StateEff to EnvEff to ReaderT Pipeline" $
        whnf runReaderTComp
        stateToReaderComp
    , bench "Manual StateEff to EnvEff to ReaderT Pipeline"  $
        whnf runReaderTComp
        stateEffToEnvEffToReaderTComp
    , bench "Manual StateEff to ReaderT Pipeline"  $
        whnf runReaderTComp
        stateEffToReaderTComp

    , bench "Curried ChurchMonad"  $
        whnf applyCurriedComp
        (curriedFreeComp @ChurchMonad)
    , bench "Curried FreeMonad"  $
        whnf applyCurriedComp
        (curriedFreeComp @FreeMonad)
    , bench "Curried FreerMonad"  $
        whnf applyCurriedComp
        (curriedFreeComp @FreerMonad)

    , bench "ReaderT ChurchMonad"  $
        whnf runReaderTComp
        (readerTFreeComp @ChurchMonad)
    , bench "ReaderT FreeMonad"  $
        whnf runReaderTComp
        (readerTFreeComp @FreeMonad)
    , bench "ReaderT FreerMonad"  $
        whnf runReaderTComp
        (readerTFreeComp @FreerMonad)

    ]
  ]