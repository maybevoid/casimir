{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Effect.Base.EffOps
where

import GHC.Exts (Constraint)

import Control.Effect.Base.Effect
import Control.Effect.Base.FreeEff
import Control.Effect.Base.EffFunctor

class
  ( EffFunctor ops
  , FreeEff ops
  , Functor (FreeModel ops)
  )
  => EffOps (ops :: (* -> *) -> *) where
    type family EffConstraint ops (eff :: * -> *) = (c :: Constraint) | c -> ops eff

    bindConstraint :: forall eff r .
      (Effect eff)
      => ops eff
      -> (EffConstraint ops eff => r)
      -> r
