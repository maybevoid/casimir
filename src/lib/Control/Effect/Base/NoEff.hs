{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Base.NoEff
  ( NoEff
  , NoOp (..)
  , NoModel (..)
  , NoConstraint
  )
where

import Control.Effect.Base.EffOps
import Control.Effect.Base.FreeEff
import Control.Effect.Base.EffFunctor

data NoEff where

data NoOp a (eff :: * -> *) = NoOp

data NoModel a = NoModel

class NoConstraint (eff :: * -> *) where

instance NoConstraint eff where

instance Functor NoModel where
  fmap _ _ = NoModel

instance EffFunctor (NoOp a) where
  -- type WrapComp (NoOp a) f = (NoOp (f a))

  effmap _ _ = NoOp

  -- wrapVal _ _ = NoOp

instance FreeEff NoEff where
  type Operation NoEff = (NoOp ())
  type CoOperation NoEff = NoModel

  freeMonad _ = NoOp

instance EffOps NoEff where
  type OpsConstraint NoEff eff = NoConstraint eff

  bindConstraint _ = id