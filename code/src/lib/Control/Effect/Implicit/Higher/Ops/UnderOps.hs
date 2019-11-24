{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.Ops.UnderOps
where

import Control.Effect.Implicit.Higher.Base

import Data.Kind
import qualified Control.Effect.Implicit.Base as Base

data UnderEff ops (inEff :: Type -> Type)

data UnderOps ops inEff eff where
  UnderOps
    :: forall ops inEff eff
     . (Effect eff, Effect inEff)
    => ops inEff eff
    -> UnderOps ops inEff eff

instance
  (Base.EffFunctor (ops inEff))
  => Base.EffFunctor (UnderOps ops inEff)
   where
    effmap lifter (UnderOps ops) =
      UnderOps $ Base.effmap lifter ops

instance
  (EffOps ops)
  => Base.EffOps (UnderEff ops eff) where
    type Operation (UnderEff ops eff) = UnderOps (Operation ops) eff