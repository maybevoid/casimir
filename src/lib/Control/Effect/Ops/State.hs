{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Ops.State
  ( StateOps (..)
  , StateModel (..)
  , StateEff
  , get
  , put
  , freeStateOps
  , bindStateModel
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
  -- bindModel = bindStateModel

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
  :: forall a f eff .
  (Functor f, Effect eff)
  => StateModel a ~> f
  -> StateOps a (FreeT f eff)
freeStateOps liftModel = StateOps {
  getOp = liftF $ liftModel $ GetOp id,
  putOp = \x -> liftF $ liftModel $ PutOp x id
}

bindStateModel
  :: forall eff s a b .
  (Effect eff)
  => StateModel s (eff a)
  -> (a -> eff b)
  -> StateModel s (eff b)
bindStateModel (GetOp cont1) cont2 = GetOp $
  \x -> cont1 x >>= cont2
bindStateModel (PutOp x cont1) cont2 = PutOp x $
  \_ -> cont1 () >>= cont2