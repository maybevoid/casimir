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
  (m :: Type -> Type)
  = UpperOps
    { innerOps' :: ops inEff
    , outerOps' :: ops m
    }

instance
  (Base.EffOps ops)
  => EffOps (UpperEff ops) where
    type Operation (UpperEff ops) = UpperOps (Base.Operation ops)

instance
  ( Monad m
  , EffFunctor lift ops
  )
  => EffFunctor lift (UpperOps ops m)
  where
    mmap lift (UpperOps ops1 ops2) =
      UpperOps ops1 (mmap lift ops2)


instance
  (EffFunctor lift ops)
  => HigherEffFunctor lift (UpperOps ops)
   where
    higherEffmap lift (UpperOps ops1 ops2) =
      UpperOps (mmap lift ops1) (mmap lift ops2)
