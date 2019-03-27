{-# LANGUAGE PolyKinds #-}

module Control.Effect.Implicit.Base.Tag
  ( TaggedEff
  , TaggedOps (..)
  , TaggedCoOp (..)
  , untagOps
  , untagCoOp
  , withTag
  )
where

import Data.Kind

import Control.Effect.Implicit.Base.EffFunctor
import Control.Effect.Implicit.Base.FreeOps
import Control.Effect.Implicit.Base.EffOps

data TaggedEff l ops

data TaggedOps
  :: forall k
   . k
   -> ((Type -> Type) -> Type)
   -> (Type -> Type)
   -> Type
 where
  TaggedOps
    :: forall l ops eff
      . ops eff
    -> TaggedOps l ops eff

data TaggedCoOp
  :: forall k
   . k
  -> (Type -> Type)
  -> Type
  -> Type
 where
  TaggedCoOp
    :: forall l coop r
     . coop r
    -> TaggedCoOp l coop r

instance
  (EffFunctor ops)
  => EffFunctor (TaggedOps l ops)
  where
    effmap lift (TaggedOps ops) = TaggedOps $ effmap lift ops

instance
  (Functor coop)
  => Functor (TaggedCoOp l coop)
  where
    fmap f (TaggedCoOp coop) = TaggedCoOp $ fmap f coop

instance
  (FreeOps ops)
  => FreeOps (TaggedEff l ops)
  where
    type Operation (TaggedEff l ops)
      = TaggedOps l (Operation ops)

    type CoOperation (TaggedEff l ops)
      = TaggedCoOp l (CoOperation ops)

    mkFreeOps liftCoOp = TaggedOps $ mkFreeOps (liftCoOp . TaggedCoOp)

untagOps :: forall l ops eff .
  TaggedOps l ops eff
  -> ops eff
untagOps (TaggedOps ops) = ops

untagCoOp :: forall l coop r .
  TaggedCoOp l coop r
  -> coop r
untagCoOp (TaggedCoOp coop) = coop

withTag
  :: forall l ops eff r
   . ( EffOps ops
     , EffOps (TaggedEff l ops)
     , EffConstraint (TaggedEff l ops) eff
     )
  => ((OpsConstraint ops eff) => r)
  -> r
withTag comp = withOps ops comp
 where
  ops :: Operation ops eff
  TaggedOps ops = captureOps @(TaggedEff l ops)