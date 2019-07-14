{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.LiftedOps
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.HigherOps

data LiftedEff ops

data LiftedOps ops inEff eff = HIoOps
  { innerOps' :: Operation ops inEff
  , outerOps' :: Operation ops eff
  }

instance HigherOps (LiftedEff ops) where
  type HOperation (LiftedEff ops) = LiftedOps ops

instance
  (EffOps ops, EffFunctor (Operation ops))
  => HigherEffFunctor (LiftedOps ops)
   where
    liftHigherOps _ = undefined
