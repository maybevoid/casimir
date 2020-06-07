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
  (eff :: Type -> Type)
  = LabeledOps {
    unlabelOps :: ops eff
  }

instance
  (EffOps ops)
  => EffOps (LabeledEff k label ops) where
  type Operation (LabeledEff k label ops) =
    LabeledOps k label (Operation ops)

instance
  (EffFunctor lift ops)
  => EffFunctor lift (LabeledOps k (label :: k) ops)
   where
    effmap lifter (LabeledOps ops) =
      LabeledOps $ effmap lifter ops

instance
  (EffOps ops)
  => ImplicitOps (LabeledEff k (label :: k) ops)
   where
    type OpsConstraint (LabeledEff k (label :: k) ops) eff =
      Param k label (LabeledOps k label (Operation ops) eff)

    withOps
      :: forall eff r
       . (Effect eff)
      => LabeledOps k label (Operation ops) eff
      -> (Param k label (LabeledOps k label (Operation ops) eff)
          => r)
      -> r
    withOps = withParam @k @label

    captureOps
      :: forall eff
       . ( Effect eff
         , Param k label (LabeledOps k label (Operation ops) eff)
         )
      => LabeledOps k label (Operation ops) eff
    captureOps = captureParam @k @label
