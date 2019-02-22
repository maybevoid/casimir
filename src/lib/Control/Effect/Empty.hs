
module Control.Effect.Empty where

import Control.Effect.Class

data EmptyEffRow (eff :: * -> *) = EmptyEffRow

instance EffFunctor EmptyEffRow where
  effmap _ _ = EmptyEffRow

instance EffRow EmptyEffRow where
  type EffConstraint EmptyEffRow eff = ()

  bindConstraint _ = id
