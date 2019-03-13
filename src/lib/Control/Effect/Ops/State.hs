{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Ops.State
  ( StateEff
  , StateOps (..)
  , StateModel (..)
  , StateConstraint
  , get
  , put
  , freeStateOps
  )
where

import Control.Natural (type (~>))
import Control.Monad.Trans.Free (FreeT, liftF)

import Control.Effect.Base
  ( Effect
  , EffFunctor (..)
  , FreeEff (..)
  , EffOps (..)
  , UnionOps (..)
  , Normalizable (..)
  )

import Control.Effect.Dynamic

data StateEff s where

data StateOps s eff = StateOps {
  getOp :: eff s,
  putOp :: s -> eff ()
}

data StateModel s a =
    GetOp (s -> a)
  | PutOp s (() -> a)

type StateConstraint s eff = (?stateOps :: StateOps s eff)

instance Functor (StateModel s) where
  fmap f (GetOp cont) = GetOp $ fmap f cont
  fmap f (PutOp s cont) = PutOp s $ fmap f cont

instance EffFunctor (StateOps a) where
  effmap liftEff stateOps = StateOps {
    getOp = liftEff $ getOp stateOps,
    putOp = liftEff . putOp stateOps
  }

instance FreeEff (StateEff s) where
  type Operation (StateEff s) = StateOps s
  type CoOperation (StateEff s) = StateModel s

  freeOps = freeStateOps

instance EffOps (StateEff s) where
  type OpsConstraint (StateEff s) eff = StateConstraint s eff

  bindConstraint stateOps comp = let ?stateOps = stateOps in comp

  captureOps = ?stateOps

instance DynamicOps (StateEff s) where
  dynamicOps = dynamicStateOps

instance Normalizable (StateEff s) where
  unionOps = UnionOps

get :: forall a eff .
  (StateConstraint a eff)
  => eff a
get = getOp ?stateOps

put :: forall a eff .
  (StateConstraint a eff)
  => a
  -> eff ()
put = putOp ?stateOps

freeStateOps
  :: forall a f eff .
  (Functor f, Effect eff)
  => StateModel a ~> f
  -> StateOps a (FreeT f eff)
freeStateOps liftModel = StateOps {
  getOp = liftF $ liftModel $ GetOp id,
  putOp = \x -> liftF $ liftModel $ PutOp x id
}

dynamicStateOps
  :: forall eff s .
  (Effect eff)
  => StateOps s (DynamicEff (StateEff s) eff)
dynamicStateOps = StateOps {
  getOp = liftOps $ GetOp return,
  putOp = \x -> liftOps $ PutOp x return
}