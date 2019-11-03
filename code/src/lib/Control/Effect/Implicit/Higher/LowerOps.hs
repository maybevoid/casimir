{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.LowerOps
where

import Control.Effect.Implicit.Base

data LowerOps ops eff where
  LowerOps
    :: forall ops inEff eff
     . (Effect eff, Effect inEff)
    => ops inEff eff
    -> LowerOps ops eff

instance
  ( forall inEff
     . (Effect inEff)
    => EffFunctor (ops inEff)
  )
  => EffFunctor (LowerOps ops)
   where
    effmap _ = undefined
