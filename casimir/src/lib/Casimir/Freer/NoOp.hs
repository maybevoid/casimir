{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Freer.NoOp
  ( NoCoOp
  )
where

import Casimir.Base
import Casimir.Freer.CoOp
import Casimir.Freer.FreeOps

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
