{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Implicit.Higher.Ops.UpperOps
where

import Data.Kind

import Control.Effect.Implicit.Base
  ( ContraLift (..)
  , EffFunctor (..)
  )
import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.EffFunctor

import qualified Control.Effect.Implicit.Base as Base

data UpperEff ops

data UpperOps ops
  (inEff :: Type -> Type)
  (eff :: Type -> Type)
  = UpperOps
    { innerOps' :: ops inEff
    , outerOps' :: ops eff
    }

instance
  (Base.EffOps ops)
  => EffOps (UpperEff ops) where
    type Operation (UpperEff ops) = UpperOps (Base.Operation ops)

instance
  ( Effect eff
  , EffFunctor ops
  )
  => EffFunctor (UpperOps ops eff)
  where
    effmap
      :: forall eff1 eff2
       . (Effect eff1, Effect eff2)
      => (forall x. eff1 x -> eff2 x)
      -> UpperOps ops eff eff1
      -> UpperOps ops eff eff2
    effmap lifter (UpperOps ops1 ops2) =
      UpperOps ops1 (Base.effmap lifter ops2)

instance
  (EffFunctor ops)
  => HigherEffFunctor (UpperOps ops)
   where
    invEffmap
      :: forall eff1 eff2
       . (Effect eff1, Effect eff2)
      => (forall x. eff1 x -> eff2 x)
      -> ContraLift eff1 eff2
      -> UpperOps ops eff1 eff1
      -> UpperOps ops eff2 eff2
    invEffmap lifter _ (UpperOps ops1 ops2) =
      UpperOps (Base.effmap lifter ops1) (Base.effmap lifter ops2)
