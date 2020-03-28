{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Ops.Io.Transform
where

import Control.Effect.Implicit.MonadOps

import Control.Effect.Implicit.Ops.Io.Base

data UseIo

instance HasOps UseIo where
  type SupportedOps UseIo = IoEff

instance MonadOps UseIo where
  type OpsMonad UseIo = IO

  monadOps = ioOps
