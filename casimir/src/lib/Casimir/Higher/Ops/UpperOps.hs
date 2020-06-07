{-# LANGUAGE UndecidableInstances #-}

module Casimir.Higher.Ops.UpperOps
where

import Data.Kind

import Casimir.Base
  ( EffFunctor (..)
  )
import Casimir.Higher.Base
import Casimir.Higher.EffFunctor

import qualified Casimir.Base as Base

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
  ( Monad eff
  , EffFunctor lift ops
  )
  => EffFunctor lift (UpperOps ops eff)
  where
    effmap lift (UpperOps ops1 ops2) =
      UpperOps ops1 (effmap lift ops2)


instance
  (EffFunctor lift ops)
  => HigherEffFunctor lift (UpperOps ops)
   where
    higherEffmap lift (UpperOps ops1 ops2) =
      UpperOps (effmap lift ops1) (effmap lift ops2)
