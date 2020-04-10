{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Free.NoOp
  ( NoCoOp
  )
where

import Casimir.Base
import Casimir.Free.CoOp
import Casimir.Free.FreeOps

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
