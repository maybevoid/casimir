{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Base.BaseOps
where

import Control.Effect.Implicit.Base.EffOps
import Control.Effect.Implicit.Base.EffFunctor
import Control.Effect.Implicit.Base.Implicit

class
  (ImplicitOps ops, EffFunctor (Operation ops))
  => BaseOps ops
  where

instance
  (ImplicitOps ops, EffFunctor (Operation ops))
  => BaseOps ops
  where
