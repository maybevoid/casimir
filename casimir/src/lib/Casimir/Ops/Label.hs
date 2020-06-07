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

instance
  (Effect ops)
  => Effect (LabeledEff k label ops) where
  type Operation (LabeledEff k label ops) =
    LabeledOps k label (Operation ops)

instance
  (EffFunctor lift ops)
  => EffFunctor lift (LabeledOps k (label :: k) ops)
   where
    effmap lifter (LabeledOps ops) =
      LabeledOps $ effmap lifter ops

instance
  (Effect ops)
  => ImplicitOps (LabeledEff k (label :: k) ops)
   where
    type OpsConstraint (LabeledEff k (label :: k) ops) m =
      Param k label (LabeledOps k label (Operation ops) m)

    withOps
      :: forall m r
       . (Monad m)
      => LabeledOps k label (Operation ops) m
      -> (Param k label (LabeledOps k label (Operation ops) m)
          => r)
      -> r
    withOps = withParam @k @label

    captureOps
      :: forall m
       . ( Monad m
         , Param k label (LabeledOps k label (Operation ops) m)
         )
      => LabeledOps k label (Operation ops) m
    captureOps = captureParam @k @label
