{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.UnderOps
where

import Control.Effect.Implicit.Higher.Base

import qualified Control.Effect.Implicit.Base as Base

data UnderEff ops

data UnderOps ops eff where
  UnderOps
    :: forall ops inEff eff
     . (Effect eff, Effect inEff)
    => ops inEff eff
    -> UnderOps ops eff

instance
  ( forall inEff
     . (Effect inEff)
    => Base.EffFunctor (ops inEff)
  )
  => Base.EffFunctor (UnderOps ops)
   where
    effmap lifter (UnderOps ops) =
      UnderOps $ Base.effmap lifter ops

instance
  (EffOps ops)
  => Base.EffOps (UnderEff ops) where
    type Operation (UnderEff ops) = UnderOps (Operation ops)