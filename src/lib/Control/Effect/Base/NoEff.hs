{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Base.NoEff
  ( NoEff
  , NoOp (..)
  , NoCoOp (..)
  , NoConstraint
  )
where

import Data.Kind
import Control.Effect.Base.EffOps
import Control.Effect.Base.FreeOps
import Control.Effect.Base.EffFunctor

data NoEff where

data NoOp (eff :: Type -> Type) = NoOp

data NoCoOp a = NoCoOp

class NoConstraint (eff :: Type -> Type) where

instance NoConstraint eff where

instance Functor NoCoOp where
  fmap _ _ = NoCoOp

instance EffFunctor NoOp where
  effmap _ _ = NoOp

instance FreeOps NoEff where
  type Operation NoEff = NoOp
  type CoOperation NoEff = NoCoOp

  mkFreeOps _ = NoOp

instance EffOps NoEff where
  type OpsConstraint NoEff eff = NoConstraint eff

  bindConstraint _ = id

  captureOps = NoOp