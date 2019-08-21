{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Implicit.Ops.Label
where

import Data.Kind (Type)
import GHC.Types (Symbol)

import Control.Effect.Implicit.Base

type NamedOps label ops = LabeledOps Symbol label ops
type TaggedOps label ops = LabeledOps Type label ops

newtype LabeledOps
  k (label :: k)
  ops
  (eff :: Type -> Type)
  = LabeledOps {
    unlabelOps :: ops eff
  }

instance
  (EffFunctor ops)
  => EffFunctor (LabeledOps k (label :: k) ops)
   where
    effmap :: forall eff1 eff2 .
      (Effect eff1, Effect eff2)
      => (forall x . eff1 x -> eff2 x)
      -> LabeledOps k label ops eff1
      -> LabeledOps k label ops eff2
    effmap lifter (LabeledOps ops) = LabeledOps $ effmap lifter ops

instance ImplicitOps (LabeledOps k (label :: k) ops)
   where
    type OpsConstraint (LabeledOps k (label :: k) ops) eff =
      ImplicitParam k label (LabeledOps k label ops eff)

    withOps
      :: forall eff r
       . (Effect eff)
      => LabeledOps k label ops eff
      -> (ImplicitParam k label (LabeledOps k label ops eff) => r)
      -> r
    withOps = withParam @k @label

    captureOps
      :: forall eff
       . (Effect eff, ImplicitParam k label (LabeledOps k label ops eff))
      => LabeledOps k label ops eff
    captureOps = captureParam @k @label
