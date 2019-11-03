
module Control.Effect.Implicit.Higher.CoOp
where

import Data.Kind

class EffCoOp ops where
  type family CoOperation ops =
    ( coop
      :: (Type -> Type)
      -> Type
      -> Type
    ) | coop -> ops

class
  (forall f . (Functor f) => Functor (coop f))
  => CoOpFunctor coop where
    liftCoOp
      :: forall f1 f2 a
        . (Functor f1, Functor f2)
      => (forall x . f1 x -> f2 x)
      -> coop f1 a
      -> coop f2 a
