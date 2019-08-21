{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.LowerOps
where

import Data.Kind
import Control.Effect.Implicit.Base

data InnerOps hops inOps
  (inEff :: Type -> Type)
  (eff :: Type -> Type)
  = InnerOps
    { otherOps :: inOps inEff
    , innerOps :: hops inEff inEff
    , outerOps :: hops inEff eff
    }

data LowerOps hops inOps eff where
  LowerOps
    :: forall inEff hops inOps eff
     . (Effect eff)
    => InnerOps hops inOps inEff eff
    -> LowerOps hops inOps eff

instance
  ( forall inEff
     . (Effect inEff)
    => EffFunctor (hops inEff)
  , EffFunctor inOps
  )
  => EffFunctor (LowerOps hops inOps)
   where
    effmap _ = undefined
