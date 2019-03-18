{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where
import Criterion.Main

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import Control.Effect

import Benchmark.State

applyCurriedComp
  :: (Int -> Computation NoEff (Return a) Identity)
  -> a
applyCurriedComp comp = runIdentity $ returnVal $
  runComp (comp 5) idLift NoOp

runReaderTComp
  :: ReaderT Int Identity ()
  -> ()
runReaderTComp m =runIdentity $ runReaderT m 5

main :: IO ()
main = defaultMain [
  bgroup "State Benchmark"
    [ bench "With StateT Handler"  $
        nf (\m -> runIdentity $ evalStateT m 5) stateTComp1
    -- , bench "With StateT Computation" $
    --     nf runReaderTComp withStateTComp
    -- , bench "Bind StateT Handler Computation"  $
    --     nf (\m -> runIdentity $ evalStateT m 5) stateTHandlerComp
    -- , bench "Transformer StateEff to EnvEff to ReaderT Pipeline" $
    --     nf runReaderTComp stateToReaderComp
    -- , bench "Manual StateEff to EnvEff to ReaderT Pipeline"  $
    --     nf runReaderTComp stateEffToEnvEffToReaderTComp
    -- , bench "Manual StateEff to ReaderT Pipeline"  $
    --     nf runReaderTComp stateEffToReaderTComp
    , bench "ReaderT FreeMonad"  $
        nf runReaderTComp (readerTFreeComp @FreeMonad)
    , bench "ReaderT FreerMonad"  $
        nf runReaderTComp (readerTFreeComp @FreerMonad)
    , bench "ReaderT ChurchMonad"  $
        nf
        runReaderTComp
        (readerTFreeComp @ChurchMonad)
    , bench "Curried FreeMonad"  $
        nf
        applyCurriedComp
        (curriedFreeComp @FreeMonad)
    , bench "Curried FreerMonad"  $
        nf applyCurriedComp (curriedFreeComp @FreerMonad)
    , bench "Curried ChurchMonad"  $
        nf applyCurriedComp (curriedFreeComp @ChurchMonad)
    ]
  ]