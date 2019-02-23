
module Control.Effect.Union where

import Control.Effect.Class

data Union f g eff where
  Union :: (EffOps f, EffOps g) => f eff -> g eff -> Union f g eff

instance EffFunctor (Union f g) where
  effmap f (Union x y) = Union (effmap f x) (effmap f y)

instance (EffOps f, EffOps g) => EffOps (Union f g) where
  type EffConstraint (Union f g) eff = (EffConstraint f eff, EffConstraint g eff)

  bindConstraint (Union x y) comp =
    bindConstraint x $ bindConstraint y comp
