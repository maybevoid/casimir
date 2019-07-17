{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.LowerOps
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.HigherOps

data LowerEff hops inOps

data InnerOps hops inOps inEff eff = InnerOps
  { otherOps :: Operation inOps inEff
  , innerOps :: HOperation hops inEff inEff
  , outerOps :: HOperation hops inEff eff
  }

data LowerOps hops inOps eff where
  LowerOps
    :: forall inEff hops inOps eff
     . (Effect eff)
    => InnerOps hops inOps inEff eff
    -> LowerOps hops inOps eff

instance
  (HigherOps hops, EffOps inOps)
  => EffOps (LowerEff hops inOps)
   where
    type Operation (LowerEff hops inOps) =
      LowerOps hops inOps

instance
  ( forall inEff
     . (Effect inEff)
    => EffFunctor (HOperation hops inEff)
  , EffFunctor (Operation inOps)
  )
  => EffFunctor (LowerOps hops inOps)
   where
    effmap _ = undefined
