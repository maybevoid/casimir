{-# LANGUAGE UndecidableInstances #-}

module Casimir.Base.EffFunctor
  ( EffFunctor (..)
  )
where

import Data.Kind

import Casimir.Base.Lift
import Casimir.Base.Effect

class EffFunctor
  (lift :: (Type -> Type) -> (Type -> Type) -> Type)
  (comp :: (Type -> Type) -> Type)
  where
    effmap
      :: forall m1 m2
      .  (Monad m1, Monad m2)
      => lift m1 m2
      -> comp m1
      -> comp m2

instance {-# INCOHERENT #-}
  ( EffFunctor Lift comp
  )
  => EffFunctor HigherLift comp where
    effmap
      :: forall m1 m2
       . (Monad m1, Monad m2)
      => HigherLift m1 m2
      -> comp m1
      -> comp m2
    effmap (HigherLift lift _) comp =
      effmap (Lift lift) comp

instance EffFunctor Lift Nil where
  effmap _ _ = Nil

instance {-# INCOHERENT #-}
  ( EffFunctor lift ops1
  , EffFunctor lift ops2
  )
  => EffFunctor lift (Union ops1 ops2)
  where
    effmap f (Union x y)
      = Union (effmap f x) (effmap f y)
