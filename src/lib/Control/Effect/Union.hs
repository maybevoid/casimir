
module Control.Effect.Union where

import Control.Effect.Class

data UnionEffRow f g eff where
  UnionEffRow :: (EffRow f, EffRow g) => f eff -> g eff -> UnionEffRow f g eff

instance EffFunctor (UnionEffRow f g) where
  effmap f (UnionEffRow x y) = UnionEffRow (effmap f x) (effmap f y)

instance (EffRow f, EffRow g) => EffRow (UnionEffRow f g) where
  type EffConstraint (UnionEffRow f g) eff = (EffConstraint f eff, EffConstraint g eff)

  bindConstraint (UnionEffRow x y) comp =
    bindConstraint x $ bindConstraint y comp
