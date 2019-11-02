{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.UpperOps
where

import Data.Kind

import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.EffFunctor

import qualified Control.Effect.Implicit.Base as Base

data UpperOps ops
  (inEff :: Type -> Type)
  (eff :: Type -> Type)
  = UpperOps
    { innerOps' :: Base.Operation ops inEff
    , outerOps' :: Base.Operation ops eff
    }

instance
  ( Effect eff
  , Base.EffOps ops
  , Base.EffFunctor (Base.Operation ops)
  )
  => Base.EffFunctor (UpperOps ops eff)
  where
    effmap _ = undefined

instance
  ( Effect eff
  , Base.EffOps ops
  , Base.EffFunctor (Base.Operation ops)
  )
  => EffFunctor (UpperOps ops)
   where
    invEffmap _ = undefined
