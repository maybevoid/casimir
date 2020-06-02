
module Casimir.Ops.State.Base
  ( StateEff
  , StateOps (..)
  )
where

import Data.QuasiParam.Tag
import Casimir.Base

data StateEff s

data StateOps s m
  = StateOps
    { getOp :: m s
    , putOp :: s -> m ()
    }

instance EffOps (StateEff s) where
  type Operation (StateEff s) = StateOps s

instance EffFunctor Lift (StateOps a) where
  effmap (Lift lift) stateOps = StateOps
    { getOp = lift $ getOp stateOps
    , putOp = lift . putOp stateOps
    }
