{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where
import Criterion.Main

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import Control.Effect

import Benchmark.State

main :: IO ()
main = defaultMain [
  bgroup "State Benchmark"
    [ bench "With StateT Handler"  $
        nf (\m -> runIdentity $ evalStateT m 5) stateTComp1
    , bench "With StateT Computation" $
        nf (\m -> runIdentity $ runReaderT m 5) withStateTComp
    , bench "Bind StateT Handler Computation"  $
        nf (\m -> runIdentity $ evalStateT m 5) stateTHandlerComp
    , bench "Transformer StateEff to EnvEff to ReaderT Pipeline" $
        nf (\m -> runIdentity $ runReaderT m 5) stateToReaderComp
    , bench "Manual StateEff to EnvEff to ReaderT Pipeline"  $
        nf (\m -> runIdentity $ runReaderT m 5) stateEffToEnvEffToReaderTComp
    , bench "Manual StateEff to ReaderT Pipeline"  $
        nf (\m -> runIdentity $ runReaderT m 5) stateEffToReaderTComp
    , bench "ReaderT FreeMonad"  $
        nf (\m -> runIdentity $ runReaderT m 5) (readerTFreeComp @FreeMonad)
    , bench "ReaderT FreerMonad"  $
        nf (\m -> runIdentity $ runReaderT m 5) (readerTFreeComp @FreerMonad)
    , bench "ReaderT ChurchMonad"  $
        nf (\m -> runIdentity $ runReaderT m 5) (readerTFreeComp @ChurchMonad)
    , bench "Curried FreeMonad"  $
        nf applyCurriedComp (curriedFreeComp @FreeMonad)
    , bench "Curried FreerMonad"  $
        nf applyCurriedComp (curriedFreeComp @FreerMonad)
    , bench "Curried ChurchMonad"  $
        nf applyCurriedComp (curriedFreeComp @ChurchMonad)
    ]
  ]