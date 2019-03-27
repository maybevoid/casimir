
module Control.Effect.Implicit.Ops.State
  ( StateEff
  , StateOps (..)
  , StateCoOp (..)
  , StateConstraint
  , get
  , put
  )
where

import Control.Effect.Implicit.Base
  ( EffFunctor (..)
  , FreeOps (..)
  , EffOps (..)
  )

data StateEff s where

data StateOps s eff = StateOps {
  getOp :: eff s,
  putOp :: s -> eff ()
}

data StateCoOp s a =
    GetOp (s -> a)
  | PutOp s (() -> a)

type StateConstraint s eff = (?stateOps :: StateOps s eff)

instance Functor (StateCoOp s) where
  fmap f (GetOp cont) = GetOp $ fmap f cont
  fmap f (PutOp s cont) = PutOp s $ fmap f cont

instance EffFunctor (StateOps a) where
  effmap liftEff stateOps = StateOps {
    getOp = liftEff $ getOp stateOps,
    putOp = liftEff . putOp stateOps
  }

instance FreeOps (StateEff s) where
  type Operation (StateEff s) = StateOps s
  type CoOperation (StateEff s) = StateCoOp s

  mkFreeOps liftCoOp = StateOps {
    getOp = liftCoOp $ GetOp id,
    putOp = \x -> liftCoOp $ PutOp x id
  }

instance EffOps (StateEff s) where
  type OpsConstraint (StateEff s) eff = StateConstraint s eff

  {-# INLINE withOps #-}
  withOps stateOps comp = let ?stateOps = stateOps in comp

  {-# INLINE captureOps #-}
  captureOps = ?stateOps

{-# INLINE get #-}
get :: forall a eff .
  (StateConstraint a eff)
  => eff a
get = getOp ?stateOps

{-# INLINE put #-}
put :: forall a eff .
  (StateConstraint a eff)
  => a
  -> eff ()
put = putOp ?stateOps
