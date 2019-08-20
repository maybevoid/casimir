
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

import qualified Control.Effect.Implicit.Free as Free
import qualified Control.Effect.Implicit.Freer as Freer

data StateTag
data StateEff s where

data StateOps s eff = StateOps {
  getOp :: eff s,
  putOp :: s -> eff ()
}

data StateCoOp s a =
    GetOp (s -> a)
  | PutOp s (() -> a)

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
    (TaggedOpsParam StateTag (StateEff s) eff)

  withOps = withTag @StateTag
  captureOps = captureTag @StateTag

{-# INLINE get #-}
get :: forall s . Eff (StateEff s) s
get = getOp captureOps

{-# INLINE put #-}
put :: forall s . s -> Eff (StateEff s) ()
put = putOp captureOps
