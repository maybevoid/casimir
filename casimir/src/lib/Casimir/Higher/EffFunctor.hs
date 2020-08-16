{-# LANGUAGE AllowAmbiguousTypes #-}

module Casimir.Higher.EffFunctor where

import Casimir.Higher.Lift
import Casimir.Higher.Params
import Casimir.Higher.ArgKind

import Data.Kind (Type)

class EffFunctor
  (lift :: MonadPair -> MonadPair -> Type)
  (ops :: MonadPair -> Type)
  where
    effmap
      :: forall m11 m12 m21 m22
       . ( Monad m11
         , Monad m12
         , Monad m21
         , Monad m22
         )
      => lift ('MonadPair m11 m12) ('MonadPair m21 m22)
      -> Operation ops m11 m12
      -> Operation ops m21 m22

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
