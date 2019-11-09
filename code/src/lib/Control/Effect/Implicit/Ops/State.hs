
module Control.Effect.Implicit.Ops.State
  ( StateEff
  , StateOps (..)
  , StateCoOp (..)
  , FreerStateCoOp (..)
  , get
  , put
  )
where

import Control.Implicit.Param
import Control.Effect.Implicit.Base

import qualified Control.Effect.Implicit.Higher as Higher

import qualified Control.Effect.Implicit.Free as Free
import qualified Control.Effect.Implicit.Freer as Freer

data StateTag
data StateEff s

data StateOps s eff = StateOps {
  getOp :: eff s,
  putOp :: s -> eff ()
}

data StateCoOp s r =
    GetOp (s -> r)
  | PutOp s (() -> r)

data FreerStateCoOp s a where
  GetOp' :: FreerStateCoOp s s
  PutOp' :: s -> FreerStateCoOp s ()

instance EffOps (StateEff s) where
  type Operation (StateEff s) = StateOps s

instance Free.EffCoOp (StateEff s) where
  type CoOperation (StateEff s) = StateCoOp s

instance Freer.EffCoOp (StateEff s) where
  type CoOperation (StateEff s) = FreerStateCoOp s

instance Functor (StateCoOp s) where
  fmap f (GetOp cont) = GetOp $ fmap f cont
  fmap f (PutOp s cont) = PutOp s $ fmap f cont

instance EffFunctor (StateOps a) where
  effmap lifter stateOps = StateOps {
    getOp = lifter $ getOp stateOps,
    putOp = lifter . putOp stateOps
  }

instance Free.FreeOps (StateEff s) where
  mkFreeOps liftCoOp = StateOps {
    getOp = liftCoOp $ GetOp id,
    putOp = \x -> liftCoOp $ PutOp x id
  }

instance Freer.FreeOps (StateEff s) where
  mkFreeOps liftCoOp = StateOps {
    getOp = liftCoOp $ GetOp',
    putOp = \x -> liftCoOp $ PutOp' x
  }

instance ImplicitOps (StateEff s) where
  type OpsConstraint (StateEff s) eff =
    TaggedParam StateTag (StateOps s eff)

  withOps = withTag @StateTag
  captureOps = captureTag @StateTag

instance Higher.EffOps (StateEff s) where
  type Operation (StateEff s) = Higher.HigherOps (StateOps s)

instance Higher.EffCoOp (StateEff s) where
  type CoOperation (StateEff s) = Higher.HigherCoOp (FreerStateCoOp s)

instance Higher.HigherEffOps (StateEff s)
instance Higher.HigherEffCoOp (StateEff s)

instance Higher.ImplicitOps (StateEff s) where
  type OpsConstraint (StateEff s) eff1 eff2 =
    TaggedParam StateTag
      (Higher.HigherOps (StateOps s) eff1 eff2)

  withHigherOps = withTag @StateTag
  captureHigherOps = captureTag @StateTag

{-# INLINE get #-}
get :: forall s . Eff (StateEff s) s
get = getOp captureOps

{-# INLINE put #-}
put :: forall s . s -> Eff (StateEff s) ()
put = putOp captureOps
