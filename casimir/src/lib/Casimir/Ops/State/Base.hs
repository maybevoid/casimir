
module Casimir.Ops.State.Base
  ( StateTag
  , State
  , StateOps (..)
  , get
  , put
  )
where

import QuasiParam.Tag
import Casimir.Base

data StateTag
data State s

data StateOps s m = StateOps {
  getOp :: m s,
  putOp :: s -> m ()
}

instance Effect (State s) where
  type Operation (State s) = StateOps s

instance EffFunctor Lift (StateOps a) where
  effmap (Lift lift) stateOps = StateOps {
    getOp = lift $ getOp stateOps,
    putOp = lift . putOp stateOps
  }

instance ImplicitOps (State s) where
  type OpsConstraint (State s) m =
    Param StateTag (StateOps s m)

  withOps = withParam @StateTag
  captureOps = captureParam @StateTag

{-# INLINE get #-}
get :: forall s . Eff (State s) s
get = getOp captureOps

{-# INLINE put #-}
put :: forall s . s -> Eff (State s) ()
put = putOp captureOps
