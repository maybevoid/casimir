{-|
  Module      : Control.Effect.Implicit
  Description : Algebraic Effects in Haskell using Implicit Parameters
  Copyright   : (c) Soares Ruofei Chen, 2013
  License     : BSD-3
  Maintainer  : soares.chen@maybevoid.com
  Stability   : experimental
  Portability : POSIX

  `implicit-effects` is a experimental effect library to support algebraic
  effects in Haskell.
-}

module Control.Effect.Implicit
  ( module Control.Effect.Implicit.Base
  , module Control.Effect.Implicit.Computation
  , module Control.Effect.Implicit.Free
  , module Control.Effect.Implicit.Transform
  , module Control.Effect.Implicit.Tag
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation
import Control.Effect.Implicit.Free
import Control.Effect.Implicit.Transform
import Control.Effect.Implicit.Tag