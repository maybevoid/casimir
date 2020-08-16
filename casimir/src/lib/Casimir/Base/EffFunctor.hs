{-# LANGUAGE AllowAmbiguousTypes #-}

module Casimir.Base.EffFunctor where

import Casimir.Base.Params
import Casimir.Base.Lift

import Data.Kind (Type)

class EffFunctor
  (lift :: (Type -> Type) -> (Type -> Type) -> Type)
  (ops :: (Type -> Type) -> Type)
  where
    effmap
      :: forall m1 m2
      .  (Monad m1, Monad m2)
      => lift m1 m2
      -> ops m1
      -> ops m2

instance {-# INCOHERENT #-}
  EffFunctor lift Nil
  where
    effmap _ _ = Nil

instance {-# INCOHERENT #-}
  ( EffFunctor lift ops1
  , EffFunctor lift ops2
  )
  => EffFunctor lift (Union ops1 ops2)
  where
    effmap f (Union x y)
      = Union (effmap f x) (effmap f y)

instance {-# INCOHERENT #-}
  ( LiftFunctor lift1 lift2
  , EffFunctor lift2 ops
  )
  => EffFunctor lift1 ops where
    effmap lift = effmap (transformLift @lift1 @lift2 lift)
