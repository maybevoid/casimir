{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Ops.Io.Transform
where

import Casimir.MonadOps

import Casimir.Ops.Io.Base

data UseIo

instance HasOps UseIo where
  type SupportedOps UseIo = IoOps

instance MonadOps UseIo where
  type OpsMonad UseIo = IO

  monadOps = ioOps
