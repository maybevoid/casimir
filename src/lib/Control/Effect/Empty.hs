
module Control.Effect.Empty where

import Control.Effect.Class

data EmptyRow (eff :: * -> *) = EmptyRow

instance EffFunctor EmptyRow where
  effmap _ _ = EmptyRow

instance EffRow EmptyRow where
  type EffConstraint EmptyRow eff = ()

  bindConstraint _ = id
