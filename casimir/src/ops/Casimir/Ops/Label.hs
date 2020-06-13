{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Casimir.Ops.Label
where

import Data.Kind (Type)
import GHC.Types (Symbol)
import QuasiParam.Label

import Casimir.Base

type NamedOps label ops = LabeledOps Symbol label ops
type TaggedOps label ops = LabeledOps Type label ops

data LabeledEff k (label :: k) ops

newtype LabeledOps
  k (label :: k)
  ops
  (m :: Type -> Type)
  = LabeledOps {
    unlabelOps :: ops m
  }

instance HasLabel (LabeledOps k (label :: k) ops) where
  type GetLabel (LabeledOps k label ops) = Label k label

instance
  ( Effect ops )
  => Effect (LabeledEff k label ops) where
  type Operation (LabeledEff k label ops) =
    LabeledOps k label (Operations ops)

instance
  (EffFunctor lift ops)
  => EffFunctor lift (LabeledOps k (label :: k) ops)
   where
    effmap lifter (LabeledOps ops) =
      LabeledOps $ effmap lifter ops
