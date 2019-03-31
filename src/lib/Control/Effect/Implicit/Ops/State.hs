
module Control.Effect.Implicit.Ops.State
  ( StateEff
  , StateOps (..)
  , StateCoOp (..)
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

instance EffOps (StateEff s) where
  type Operation (StateEff s) = StateOps s

instance EffCoOp (StateEff s) where
  type CoOperation (StateEff s) = StateCoOp s

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
  type OpsConstraint (StateEff s) eff =
    (?_Control_Effect_Implicit_Ops_State_stateOps :: StateOps s eff)

  {-# INLINE withOps #-}
  withOps stateOps comp =
    let
      ?_Control_Effect_Implicit_Ops_State_stateOps =
        stateOps in comp

  {-# INLINE captureOps #-}
  captureOps =
    ?_Control_Effect_Implicit_Ops_State_stateOps

{-# INLINE get #-}
get :: forall s eff .
  (EffConstraint (StateEff s) eff)
  => eff s
get = getOp captureOps

{-# INLINE put #-}
put :: forall s eff .
  (EffConstraint (StateEff s) eff)
  => s
  -> eff ()
put = putOp captureOps
