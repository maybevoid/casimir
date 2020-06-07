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
  (Effects ops)
  => Effects (LabeledEff k label ops) where
  type Operations (LabeledEff k label ops) =
    LabeledOps k label (Operations ops)

instance
  (EffFunctor lift ops)
  => EffFunctor lift (LabeledOps k (label :: k) ops)
   where
    effmap lifter (LabeledOps ops) =
      LabeledOps $ effmap lifter ops

instance
  (Effects ops)
  => ImplicitOps (LabeledEff k (label :: k) ops)
   where
    type OpsConstraint (LabeledEff k (label :: k) ops) m =
      Param k label (LabeledOps k label (Operations ops) m)

    withOps
      :: forall m r
       . (Monad m)
      => LabeledOps k label (Operations ops) m
      -> (Param k label (LabeledOps k label (Operations ops) m)
          => r)
      -> r
    withOps = withParam @k @label

    captureOps
      :: forall m
       . ( Monad m
         , Param k label (LabeledOps k label (Operations ops) m)
         )
      => LabeledOps k label (Operations ops) m
    captureOps = captureParam @k @label
