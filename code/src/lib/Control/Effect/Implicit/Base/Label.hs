{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Effect.Implicit.Base.Label
where

import GHC.Types
import Unsafe.Coerce
import Data.Constraint

import Control.Effect.Implicit.Base.Effect
import Control.Effect.Implicit.Base.EffOps
import Control.Effect.Implicit.Base.Implicit
import Control.Effect.Implicit.Base.EffFunctor

data LabeledEff (label :: k) ops

type NamedEff (label :: Symbol) ops = LabeledEff label ops
type TaggedEff (label :: Type) ops = LabeledEff label ops

newtype LabeledOps (label :: k) ops eff = LabeledOps {
  unlabelOps :: Operation ops eff
}

class LabelConstraint k (label :: k) ops eff
    | label -> ops eff
  where
    captureLabel :: Operation ops eff

type TagConstraint (label :: Type) = LabelConstraint Type label
type NameConstraint (label :: Symbol) = LabelConstraint Symbol label

data OpsReflector k (label :: k) ops eff = OpsReflector {
  reflectOps :: Operation ops eff
}

withLabel
  :: forall (label :: k) ops eff r
   . (Effect eff, EffOps ops)
  => Operation ops eff
  -> ((LabelConstraint k (label :: k) ops eff) => r)
  -> r
withLabel ops cont = case dict of Dict -> cont
 where
  dict :: Dict (LabelConstraint k label ops eff)
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
      -> LabeledOps label ops eff1
      -> LabeledOps label ops eff2
    effmap lifter (LabeledOps ops) = LabeledOps $ effmap lifter ops

instance
  (EffOps ops)
  => ImplicitOps (LabeledEff (label :: k) ops)
   where
    type OpsConstraint (LabeledEff (label :: k) ops) eff =
      LabelConstraint k label (LabeledEff label ops) eff

    withOps
      :: forall eff r
       . (Effect eff)
      => LabeledOps label ops eff
      -> (LabelConstraint k label (LabeledEff label ops) eff => r)
      -> r
    withOps = withLabel @k @label

    captureOps
      :: forall eff
       . (Effect eff, LabelConstraint k label (LabeledEff label ops) eff)
      => LabeledOps label ops eff
    captureOps = captureLabel @k @label

withTag
  :: forall label ops eff r
    . ( Effect eff
      , EffOps ops
      )
  => Operation ops eff
  -> (LabelConstraint Type label ops eff => r)
  -> r
withTag = withLabel @Type @label

captureTag
  :: forall label ops eff
   . ( Effect eff
     , EffOps ops
     , LabelConstraint Type label ops eff
     )
  => Operation ops eff
captureTag = captureLabel @Type @label

withName
  :: forall (label :: Symbol) ops eff r
    . ( Effect eff
      , EffOps ops
      )
  => Operation ops eff
  -> (LabelConstraint Symbol label ops eff => r)
  -> r
withName = withLabel @Symbol @label

captureName
  :: forall (label :: Symbol) ops eff
   . ( Effect eff
     , EffOps ops
     , LabelConstraint Symbol label ops eff
     )
  => Operation ops eff
captureName = captureLabel @Symbol @label
