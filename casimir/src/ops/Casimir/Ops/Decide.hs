
module Casimir.Ops.Decide
where

import QuasiParam.Tag
import Casimir.Base

data DecideTag
data DecideEff s

data DecideOps s m = DecideOps {
  decideOp :: m s
}

data DecideCoOp s a = DecideOp (s -> a)
  deriving (Functor)

instance Effects (DecideEff s) where
  type Operations (DecideEff s) = DecideOps s

instance EffFunctor Lift (DecideOps s) where
  effmap (Lift lift) ops = DecideOps {
    decideOp = lift $ decideOp ops
  }

instance HasLabel (DecideOps s) where
  type GetLabel (DecideOps s) = Tag DecideTag

decide :: forall a . Eff (DecideEff a) a
decide = decideOp captureOps
