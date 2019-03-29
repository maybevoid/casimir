
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

data StateEff s where

data StateOps s eff = StateOps {
  getOp :: eff s,
  putOp :: s -> eff ()
}

data StateCoOp s a =
    GetOp (s -> a)
  | PutOp s (() -> a)

instance EffSpec (StateEff s) where
  type Operation (StateEff s) = StateOps s
  type CoOperation (StateEff s) = StateCoOp s

type StateConstraint s eff = (?stateOps :: StateOps s eff)

instance Functor (StateCoOp s) where
  fmap f (GetOp cont) = GetOp $ fmap f cont
  fmap f (PutOp s cont) = PutOp s $ fmap f cont

instance EffFunctor (StateOps a) where
  effmap lifter stateOps = StateOps {
    getOp = lifter $ getOp stateOps,
    putOp = lifter . putOp stateOps
  }

instance FreeOps (StateEff s) where
  mkFreeOps liftCoOp = StateOps {
    getOp = liftCoOp $ GetOp id,
    putOp = \x -> liftCoOp $ PutOp x id
  }

instance ImplicitOps (StateEff s) where
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
