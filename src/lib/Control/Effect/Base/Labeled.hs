{-# LANGUAGE PolyKinds #-}

module Control.Effect.Base.Labeled
where

import Data.Kind

import Control.Effect.Base.Effect
import Control.Effect.Base.EffFunctor
import Control.Effect.Base.FreeOps
import Control.Effect.Base.EffOps

data LabeledEff l ops where

data LabeledOps
  :: forall k
   . k
   -> ((Type -> Type) -> Type)
   -> (Type -> Type)
   -> Type
 where
  LabeledOps
    :: forall l ops eff
      . ops eff
    -> LabeledOps l ops eff

data LabeledCoOp
  :: forall k
   . k
  -> (Type -> Type)
  -> Type
  -> Type
 where
  LabeledCoOp
    :: forall l coop r
     . coop r
    -> LabeledCoOp l coop r

instance
  (EffFunctor ops)
  => EffFunctor (LabeledOps l ops)
  where
    effmap lift (LabeledOps ops) = LabeledOps $ effmap lift ops

instance
  (Functor coop)
  => Functor (LabeledCoOp l coop)
  where
    fmap f (LabeledCoOp coop) = LabeledCoOp $ fmap f coop

instance
  (FreeOps ops)
  => FreeOps (LabeledEff l ops)
  where
    type Operation (LabeledEff l ops)
      = LabeledOps l (Operation ops)

    type CoOperation (LabeledEff l ops)
      = LabeledCoOp l (CoOperation ops)

    mkFreeOps liftCoOp = LabeledOps $ mkFreeOps (liftCoOp . LabeledCoOp)

withLabel
  :: forall l ops eff r
   . ( Effect eff
     , EffOps ops
     , EffOps (LabeledEff l ops)
     , OpsConstraint (LabeledEff l ops) eff
     )
  => ((OpsConstraint ops eff) => r)
  -> r
withLabel comp = withOps ops comp
 where
  ops :: Operation ops eff
  LabeledOps ops = captureOps @(LabeledEff l ops)