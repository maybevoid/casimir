{-|
  Module      : Casimir
  Description : Algebraic Monads in Haskell using Implicit Parameters
  Copyright   : (c) Soares Ruofei Chen, 2019
  License     : BSD-3
  Maintainer  : soares.chen@maybevoid.com
  Stability   : experimental
  Portability : POSIX

  This is the main module for implicit-effects. It re-exports the
  following submodules for convenience:

    - "Casimir.Base"
    - "Casimir.Computation"
    - "Casimir.Free"

  The following submodules and their descendents require manual import:

    - "Casimir.Ops".*
    - "Casimir.Transform".*

  Refer the submodules for complete documentation.
-}

module Casimir
  ( module Casimir.Base
  , module Casimir.Computation
  , module Casimir.Cast
  )
where

import Casimir.Base
import Casimir.Computation
import Casimir.Cast
