{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Ops.State
  ( StateOps (..)
  , StateModel (..)
  , StateEff
  , get
  , put
  , freeStateOps
  )
where

import Control.Natural (type (~>))
import Control.Monad.Trans.Free (FreeT, liftF)

import Control.Effect.Class
  ( Effect
  , EffFunctor (..)
  , FreeEff (..)
  , EffOps (..)
  , liftEff
  )

data StateOps s eff = StateOps {
  getOp :: eff s,
  putOp :: s -> eff ()
}

data StateModel s a =
    GetOp (s -> a)
  | PutOp s (() -> a)

type StateEff s eff = (?stateOps :: StateOps s eff)

instance Functor (StateModel s) where
  fmap f (GetOp cont) = GetOp $ fmap f cont
  fmap f (PutOp s cont) = PutOp s $ fmap f cont

instance EffFunctor (StateOps a) where
  effmap f stateOps = StateOps {
    getOp = liftEff f $ getOp stateOps,
    putOp = liftEff f . putOp stateOps
  }

instance FreeEff (StateOps s) where
  type FreeModel (StateOps s) = StateModel s

  freeModel = freeStateOps

instance EffOps (StateOps s) where
  type EffConstraint (StateOps s) eff = StateEff s eff

  bindConstraint stateOps comp = let ?stateOps = stateOps in comp

get :: forall a eff .
  (StateEff a eff)
  => eff a
get = getOp ?stateOps

put :: forall a eff .
  (StateEff a eff)
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
