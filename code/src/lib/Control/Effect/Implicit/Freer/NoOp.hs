{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Freer.NoOp
  ( NoCoOp
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Freer.CoOp
import Control.Effect.Implicit.Freer.FreeOps

-- | @'CoOperation' 'NoOp' r@ is really just @()@ for all return type @r@. We instead define
-- 'NoCoOp' with phantom type @r@ so that the injectivity condition for
-- 'CoOperation' can be satisfied.
data NoCoOp r

instance EffCoOp NoEff where
  type CoOperation NoEff = NoCoOp

instance Functor NoCoOp where
  fmap _ coop = case coop of {}

instance FreeOps NoEff where
  mkFreeOps _ = NoOp
