{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Effect.Implicit.Base.LabeledOps
where

import Data.Constraint
import Unsafe.Coerce

import Control.Effect.Implicit.Base.Effect
import Control.Effect.Implicit.Base.EffOps
import Control.Effect.Implicit.Base.Implicit
import Control.Effect.Implicit.Base.EffFunctor

data LabeledEff (label :: k) ops

newtype LabeledOps (label :: k) ops eff = LabeledOps {
  unlabelOps :: Operation ops eff
}

class
  ReifiesOps k (label :: k) ops eff | label -> ops eff
   where
    reflectOps :: Operation ops eff

data OpsReflector k (label :: k) ops eff = OpsReflector {
  reflectOps'' :: Operation ops eff
}

reifyOps
  :: forall (label :: k) ops eff r
   . (Effect eff, EffOps ops)
  => Operation ops eff
  -> ((ReifiesOps k (label :: k) ops eff) => r)
  -> r
reifyOps ops cont = case dict of Dict -> cont
 where
  dict :: Dict (ReifiesOps k label ops eff)
  dict = unsafeCoerce $ OpsReflector @k @label @ops @eff ops

instance
  (EffOps ops)
  => EffOps (LabeledEff (label :: k) ops)
   where
    type Operation (LabeledEff label ops) =
      LabeledOps label ops

instance
  (EffFunctor (Operation ops))
  => EffFunctor (LabeledOps (label :: k) ops)
   where
    effmap :: forall eff1 eff2 .
      (Effect eff1, Effect eff2)
      => (forall x . eff1 x -> eff2 x)
      -> LabeledOps (label :: k) ops eff1
      -> LabeledOps (label :: k) ops eff2
    effmap lifter (LabeledOps ops) = LabeledOps $ effmap lifter ops

instance
  (EffOps ops)
  => ImplicitOps (LabeledEff (label :: k) ops)
   where
    type OpsConstraint (LabeledEff (label :: k) ops) eff =
      ReifiesOps k (label :: k) ops eff

    withOps
      :: forall eff r
       . (Effect eff)
      => LabeledOps (label :: k) ops eff
      -> (ReifiesOps k (label :: k) ops eff => r)
      -> r
    withOps (LabeledOps ops) cont = reifyOps @k @label @ops @eff ops cont

    captureOps
      :: forall eff
       . (Effect eff, ReifiesOps k (label :: k) ops eff)
      => LabeledOps (label :: k) ops eff
    captureOps = LabeledOps $ reflectOps @k @label

withLabel
  :: forall k (label :: k) ops eff r
    . ( Effect eff
      , EffOps ops
      )
  => Operation ops eff
  -> (ReifiesOps k label ops eff => r)
  -> r
withLabel ops cont = withOps @(LabeledEff label ops) (LabeledOps ops) cont

captureLabel
  :: forall k (label :: k) ops eff
   . ( Effect eff
     , EffOps ops
     , ReifiesOps k (label :: k) ops eff
     )
  => Operation ops eff
captureLabel = unlabelOps $ captureOps @(LabeledEff label ops)
