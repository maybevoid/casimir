{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Implicit.Base.EffOps
  ( EffOps
  )
where

import Control.Effect.Implicit.Base.FreeOps
import Control.Effect.Implicit.Base.Implicit

class
  ( ImplicitOps ops
  , FreeOps ops
  )
  => EffOps ops where

instance
  ( ImplicitOps ops
  , FreeOps ops
  )
  => EffOps ops where