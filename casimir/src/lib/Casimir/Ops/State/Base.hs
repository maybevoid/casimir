
module Casimir.Ops.State.Base
  ( StateTag
  , StateEff
  , StateOps (..)
  , get
  , put
  )
where

import QuasiParam.Tag
import Casimir.Base

data StateTag
data StateEff s

data StateOps s m = StateOps {
  getOp :: m s,
  putOp :: s -> m ()
}

instance EffOps (StateEff s) where
  type Operation (StateEff s) = StateOps s

instance EffFunctor Lift (StateOps a) where
  effmap (Lift lift) stateOps = StateOps {
    getOp = lift $ getOp stateOps,
    putOp = lift . putOp stateOps
  }

instance ImplicitOps (StateEff s) where
  type OpsConstraint (StateEff s) m =
    Param StateTag (StateOps s m)

  withOps = withParam @StateTag
  captureOps = captureParam @StateTag

{-# INLINE get #-}
get :: forall s . Eff (StateEff s) s
get = getOp captureOps

{-# INLINE put #-}
put :: forall s . s -> Eff (StateEff s) ()
put = putOp captureOps
