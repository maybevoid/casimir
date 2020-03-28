{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Ops.Io.Transform
where

import Control.Effect.Implicit.Base

import Control.Effect.Implicit.Ops.Io.Base

data UseIo

instance MonadOps UseIo where
  type HasOps UseIo = IoEff

  type OpsMonad UseIo = IO

  monadOps = ioOps
