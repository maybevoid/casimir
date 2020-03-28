
module Control.Effect.Implicit.Ops.State.Base
  ( StateTag
  , StateEff
  , StateOps (..)
  , get
  , put
  )
where

import Control.Implicit.Param
import Control.Effect.Implicit.Base

data StateTag
data StateEff s

data StateOps s eff = StateOps {
  getOp :: eff s,
  putOp :: s -> eff ()
}

instance EffOps (StateEff s) where
  type Operation (StateEff s) = StateOps s

instance EffFunctor (StateOps a) where
  effmap lifter stateOps = StateOps {
    getOp = lifter $ getOp stateOps,
    putOp = lifter . putOp stateOps
  }

instance ImplicitOps (StateEff s) where
  type OpsConstraint (StateEff s) eff =
    TaggedParam StateTag (StateOps s eff)

  withOps = withTag @StateTag
  captureOps = captureTag @StateTag

{-# INLINE get #-}
get :: forall s . Eff (StateEff s) s
get = getOp captureOps

{-# INLINE put #-}
put :: forall s . s -> Eff (StateEff s) ()
put = putOp captureOps
