{-# LANGUAGE UndecidableInstances #-}

module Casimir.Higher.Ops.UpperOps
where

import Data.Kind

import Casimir.Base
  ( EffFunctor (..)
  )
data UpperEff ops

data UpperOps ops
  (inEff :: Type -> Type)
  (m :: Type -> Type)
  = UpperOps
    { innerOps' :: ops inEff
    , outerOps' :: ops m
    }

instance
  ( Monad m
  , EffFunctor lift ops
  )
  => EffFunctor lift (UpperOps ops m)
  where
    effmap lift (UpperOps ops1 ops2) =
      UpperOps ops1 (effmap lift ops2)
