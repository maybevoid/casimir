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

data LabeledEff label ops

newtype LabeledOps label ops eff = LabeledOps {
  unlabelOps :: Operation ops eff
}

class
  ReifiesOps label ops eff | label -> ops eff
   where
    reflectOps :: Operation ops eff

data OpsReflector label ops eff = OpsReflector {
  reflectOps'' :: Operation ops eff
}

reifyOps
  :: forall label ops eff r
   . (Effect eff, EffOps ops)
  => Operation ops eff
  -> ((ReifiesOps label ops eff) => r)
  -> r
reifyOps ops cont = case dict of Dict -> cont
 where
  dict :: Dict (ReifiesOps label ops eff)
  dict = unsafeCoerce $ OpsReflector @label @ops @eff ops

instance
  (EffOps ops)
  => EffOps (LabeledEff label ops)
   where
    type Operation (LabeledEff label ops) =
      LabeledOps label ops

instance
  (EffFunctor (Operation ops))
  => EffFunctor (LabeledOps label ops)
   where
    effmap :: forall eff1 eff2 .
      (Effect eff1, Effect eff2)
      => (forall x . eff1 x -> eff2 x)
      -> LabeledOps label ops eff1
      -> LabeledOps label ops eff2
    effmap lifter (LabeledOps ops) = LabeledOps $ effmap lifter ops

instance
  (EffOps ops)
  => ImplicitOps (LabeledEff label ops)
   where
    type OpsConstraint (LabeledEff label ops) eff =
      ReifiesOps label ops eff

    withOps
      :: forall eff r
       . (Effect eff)
      => LabeledOps label ops eff
      -> (ReifiesOps label ops eff => r)
      -> r
    withOps (LabeledOps ops) cont = reifyOps @label @ops @eff ops cont

    captureOps
      :: forall eff
       . (Effect eff, ReifiesOps label ops eff)
      => LabeledOps label ops eff
    captureOps = LabeledOps $ reflectOps @label

withLabel
  :: forall label ops eff r
    . ( Effect eff
      , EffOps ops
      )
  => Operation ops eff
  -> (ReifiesOps label ops eff => r)
  -> r
withLabel ops cont = withOps @(LabeledEff label ops) (LabeledOps ops) cont

captureLabel
  :: forall label ops eff
   . ( Effect eff
     , EffOps ops
     , ReifiesOps label ops eff
     )
  => Operation ops eff
captureLabel = unlabelOps $ captureOps @(LabeledEff label ops)
