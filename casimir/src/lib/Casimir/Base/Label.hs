{-# LANGUAGE PolyKinds #-}

module Casimir.Base.Label
  ( LabeledEff
  , LabeledOps (..)
  , NamedOps
  , TaggedOps
  , NamedEff
  , TaggedEff
  )
where

import Data.Kind
import GHC.Types (Symbol)

import Casimir.Base.EffOps
import Casimir.Base.EffFunctor

data LabeledEff k (label :: k) ops

newtype LabeledOps
  k
  ( label :: k )
  ( ops :: (Type -> Type) -> Type )
  ( eff :: Type -> Type )
  = LabeledOps
    { unLabelOps :: ops eff }

type NamedEff label ops = LabeledEff Symbol label ops
type TaggedEff label ops = LabeledEff Type label ops

type NamedOps label ops = LabeledOps Symbol label ops
type TaggedOps label ops = LabeledOps Type label ops

instance
  (EffOps ops)
  => EffOps (LabeledEff k label ops)
  where
    type Operation (LabeledEff k label ops) =
      LabeledOps k label (Operation ops)

instance
  (EffFunctor lift ops)
  => EffFunctor lift (LabeledOps k (label :: k) ops)
   where
    effmap lifter (LabeledOps ops) =
      LabeledOps $ effmap lifter ops
