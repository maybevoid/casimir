{-# LANGUAGE PolyKinds #-}

module Control.Effect.Implicit.Base.Label
where

import GHC.Types

import Control.Implicit.Param

import Control.Effect.Implicit.Base.Effect
import Control.Effect.Implicit.Base.EffOps

type OpsParam k (label :: k) ops eff =
  ImplicitParam k label (Operation ops eff)

type TaggedOps label ops eff = OpsParam Type label ops eff
type NamedOps label ops eff = OpsParam Symbol label ops eff

captureOpsParam
  :: forall k (label :: k) ops eff
   . (OpsParam k label ops eff)
  => Operation ops eff
captureOpsParam = captureParam @k @label @(Operation ops eff)

captureTaggedOps
  :: forall label ops eff
   . ( Effect eff
     , EffOps ops
     , OpsParam Type label ops eff
     )
  => Operation ops eff
captureTaggedOps = captureOpsParam @Type @label

captureNamedOps
  :: forall (label :: Symbol) ops eff
   . ( Effect eff
     , EffOps ops
     , OpsParam Symbol label ops eff
     )
  => Operation ops eff
captureNamedOps = captureOpsParam @Symbol @label

withOpsParam
  :: forall k (label :: k) ops eff r
   . (Effect eff, EffOps ops)
  => Operation ops eff
  -> ((OpsParam k label ops eff) => r)
  -> r
withOpsParam = withParam @k @label @(Operation ops eff)

withTaggedOps
  :: forall label ops eff r
    . ( Effect eff
      , EffOps ops
      )
  => Operation ops eff
  -> (OpsParam Type label ops eff => r)
  -> r
withTaggedOps = withOpsParam @Type @label

withNamedOps
  :: forall (label :: Symbol) ops eff r
    . ( Effect eff
      , EffOps ops
      )
  => Operation ops eff
  -> (OpsParam Symbol label ops eff => r)
  -> r
withNamedOps = withOpsParam @Symbol @label
