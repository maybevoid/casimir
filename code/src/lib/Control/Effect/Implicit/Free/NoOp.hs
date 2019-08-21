{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Free.NoOp
  ( NoCoOp (..)
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Free.EffCoOp
import Control.Effect.Implicit.Free.FreeOps

-- | @'CoOperation' 'NoOp' r@ is really just @()@ for all return type @r@. We instead define
-- 'NoCoOp' with phantom type @r@ so that the injectivity condition for
-- 'CoOperation' can be satisfied.
data NoCoOp r = NoCoOp

instance EffCoOp NoOp where
  type CoOperation NoOp = NoCoOp

instance Functor NoCoOp where
  fmap _ _ = NoCoOp

instance FreeOps NoOp where
  mkFreeOps _ = NoOp
