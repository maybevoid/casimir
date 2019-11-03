{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Free.NoOp
  ( NoCoOp (..)
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Free.CoOp
import Control.Effect.Implicit.Free.FreeOps

-- | @'CoOperation' 'NoOp' r@ is really just @()@ for all return type @r@. We instead define
-- 'NoCoOp' with phantom type @r@ so that the injectivity condition for
-- 'CoOperation' can be satisfied.
data NoCoOp r = NoCoOp

instance EffCoOp NoEff where
  type CoOperation NoEff = NoCoOp

instance Functor NoCoOp where
  fmap _ _ = NoCoOp

instance FreeOps NoEff where
  mkFreeOps _ = NoOp
