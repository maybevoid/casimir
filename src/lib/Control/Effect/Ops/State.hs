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
import Control.Monad.Free (Free, liftF)

import Control.Effect.Class
  ( EffFunctor (..)
  , FreeEff (..)
  , EffOps (..)
  , liftEff
  )

data StateOps a eff = StateOps {
  getOp :: eff a,
  putOp :: a -> eff ()
}

data StateModel s a =
    GetOp (s -> a)
  | PutOp s (() -> a)

type StateEff a eff = (?stateOps :: StateOps a eff)

instance Functor (StateModel s) where
  fmap f (GetOp cont) = GetOp $ fmap f cont
  fmap f (PutOp s cont) = PutOp s $ fmap f cont

instance EffFunctor (StateOps a) where
  effmap f stateOps = StateOps {
    getOp = liftEff f $ getOp stateOps,
    putOp = liftEff f . putOp stateOps
  }

instance FreeEff (StateOps a) where
  type FreeModel (StateOps a) = StateModel a

  freeModel = freeStateOps

instance EffOps (StateOps a) where
  type EffConstraint (StateOps a) eff = StateEff a eff

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
  :: forall a f .
  (Functor f)
  => StateModel a ~> f
  -> StateOps a (Free f)
freeStateOps liftModel = StateOps {
  getOp = liftF $ liftModel $ GetOp id,
  putOp = \x -> liftF $ liftModel $ PutOp x id
}
