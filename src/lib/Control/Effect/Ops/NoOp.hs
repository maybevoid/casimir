
module Control.Effect.Ops.NoOp where

import Control.Effect.Class

data NoOp (eff :: * -> *) = NoOp

instance EffFunctor NoOp where
  effmap _ _ = NoOp

instance EffOps NoOp where
  type EffConstraint NoOp eff = ()

  bindConstraint _ = id
