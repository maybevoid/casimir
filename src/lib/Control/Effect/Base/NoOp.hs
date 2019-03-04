{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Base.NoOp
  ( NoOp (..)
  , NoModel (..)
  , NoConstraint
  )
where

import Control.Effect.Base.EffOps
import Control.Effect.Base.FreeEff
import Control.Effect.Base.EffFunctor

data NoOp (eff :: * -> *) = NoOp

-- e.g. Proxy
data NoModel a = NoModel

class NoConstraint (eff :: * -> *) where

instance NoConstraint eff where

instance Functor NoModel where
  fmap _ _ = NoModel

instance EffFunctor NoOp where
  effmap _ _ = NoOp

instance FreeEff NoOp where
  type FreeModel NoOp = NoModel

  freeModel _ = NoOp

instance EffOps NoOp where
  type EffConstraint NoOp eff = NoConstraint eff

  bindConstraint _ = id