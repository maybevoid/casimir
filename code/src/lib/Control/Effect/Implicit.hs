{-|
  Module      : Control.Effect.Implicit
  Description : Algebraic Effects in Haskell using Implicit Parameters
  Copyright   : (c) Soares Ruofei Chen, 2019
  License     : BSD-3
  Maintainer  : soares.chen@maybevoid.com
  Stability   : experimental
  Portability : POSIX

  This is the main module for implicit-effects. It re-exports the
  following submodules for convenience:

    - "Control.Effect.Implicit.Base"
    - "Control.Effect.Implicit.Computation"
    - "Control.Effect.Implicit.Free"

  The following submodules and their descendents require manual import:

    - "Control.Effect.Implicit.Ops".*
    - "Control.Effect.Implicit.TaggedOps".*
    - "Control.Effect.Implicit.Transform".*

  Refer the submodules for complete documentation.
-}

module Control.Effect.Implicit
  ( module Control.Effect.Implicit.Base
  , module Control.Effect.Implicit.Computation
  , module Control.Effect.Implicit.Cast
  , module Control.Effect.Implicit.Reflect
  , module Control.Effect.Implicit.Higher
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation
import Control.Effect.Implicit.Cast
import Control.Effect.Implicit.Reflect
import Control.Effect.Implicit.Higher
