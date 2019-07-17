{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.UpperOps
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.HigherOps

data UpperEff ops

data UpperOps ops inEff eff = UpperOps
  { innerOps' :: Operation ops inEff
  , outerOps' :: Operation ops eff
  }

instance HigherOps (UpperEff ops) where
  type HOperation (UpperEff ops) = UpperOps ops

instance
  (EffOps ops, EffFunctor (Operation ops))
  => HigherEffFunctor (UpperOps ops)
   where
    invEffmap _ = undefined
