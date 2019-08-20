{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Implicit.Ops.Label
where

import Data.Kind (Type)
import GHC.Types (Symbol)

import Control.Effect.Implicit.Base

data LabeledEff k (label :: k) ops

type NamedEff (label :: Symbol) ops = LabeledEff Symbol label ops
type TaggedEff (label :: Type) ops = LabeledEff Type label ops

newtype LabeledOps k (label :: k) ops eff = LabeledOps {
  unlabelOps :: Operation ops eff
}

instance
  (EffOps ops)
  => EffOps (LabeledEff k (label :: k) ops)
   where
    type Operation (LabeledEff k label ops) =
      LabeledOps k label ops

instance
  (EffFunctor (Operation ops))
  => EffFunctor (LabeledOps k (label :: k) ops)
   where
    effmap :: forall eff1 eff2 .
      (Effect eff1, Effect eff2)
      => (forall x . eff1 x -> eff2 x)
      -> LabeledOps k label ops eff1
      -> LabeledOps k label ops eff2
    effmap lifter (LabeledOps ops) = LabeledOps $ effmap lifter ops

instance
  (EffOps ops)
  => ImplicitOps (LabeledEff k (label :: k) ops)
   where
    type OpsConstraint (LabeledEff k (label :: k) ops) eff =
      OpsParam k label (LabeledEff k label ops) eff

    withOps
      :: forall eff r
       . (Effect eff)
      => LabeledOps k label ops eff
      -> (OpsParam k label (LabeledEff k label ops) eff => r)
      -> r
    withOps = withOpsParam @k @label

    captureOps
      :: forall eff
       . (Effect eff, OpsParam k label (LabeledEff k label ops) eff)
      => LabeledOps k label ops eff
    captureOps = captureOpsParam @k @label
