
module Casimir.Ops.State.Base
  ( StateTag
  , StateOps (..)
  , get
  , put
  )
where

import QuasiParam.Tag
import Casimir.Base

data StateTag

data StateOps s m = StateOps {
  getOp :: m s,
  putOp :: s -> m ()
}

instance EffFunctor Lift (StateOps a) where
  effmap (Lift lift) stateOps = StateOps {
    getOp = lift $ getOp stateOps,
    putOp = lift . putOp stateOps
  }

instance HasLabel (StateOps s) where
  type GetLabel (StateOps s) = Tag StateTag

{-# INLINE get #-}
get :: forall s . Eff '[StateOps s] s
get = getOp captureOp

{-# INLINE put #-}
put :: forall s . s -> Eff '[StateOps s] ()
put = putOp captureOp
