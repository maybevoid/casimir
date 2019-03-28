{-|
  Module      : Control.Effect.Implicit
  Description : Algebraic Effects in Haskell using Implicit Parameters
  Copyright   : (c) Soares Ruofei Chen, 2019
  License     : BSD-3
  Maintainer  : soares.chen@maybevoid.com
  Stability   : experimental
  Portability : POSIX

  This is the main module for implicit-effects. It re-exports the
  majority of the submodules offered by this package, other than
  "Control.Effect.Implicit.Ops".* and  "Control.Effect.Implicit.TaggedOps".*.
  Refer the submodules for complete documentation.
-}

module Control.Effect.Implicit
  ( module Control.Effect.Implicit.Base
  , module Control.Effect.Implicit.Computation
  , module Control.Effect.Implicit.Free
  , module Control.Effect.Implicit.Transform
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation
import Control.Effect.Implicit.Free
import Control.Effect.Implicit.Transform