{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Base.BaseOps
where

import Control.Effect.Implicit.Base.EffFunctor
import Control.Effect.Implicit.Base.Implicit

class
  (ImplicitOps ops, EffFunctor ops)
  => BaseOps ops
  where

instance
  (ImplicitOps ops, EffFunctor ops)
  => BaseOps ops
  where
