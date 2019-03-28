
module Control.Effect.Implicit.Base.NoEff
  ( NoEff
  , NoOp (..)
  , NoCoOp (..)
  , NoConstraint
  )
where

import Data.Kind
import Control.Effect.Implicit.Base.EffOps
import Control.Effect.Implicit.Base.FreeOps
import Control.Effect.Implicit.Base.EffFunctor

data NoEff

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

  withOps _ = id

  captureOps = NoOp