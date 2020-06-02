
module Casimir.Ops.Decide.Base
where

import Casimir.Base
import Casimir.Free

data DecideEff s

data DecideOps s m = DecideOps {
  decideOp :: m s
}

data DecideCoOp s a = DecideOp (s -> a)
  deriving (Functor)

instance EffOps (DecideEff s) where
  type Operation (DecideEff s) = DecideOps s

instance EffCoOp (DecideEff s) where
  type CoOperation (DecideEff s) = DecideCoOp s

instance EffFunctor Lift (DecideOps s) where
  effmap (Lift lift) ops = DecideOps {
    decideOp = lift $ decideOp ops
  }

instance FreeOps (DecideEff s) where
  mkFreeOps liftCoOp = DecideOps {
    decideOp = liftCoOp $ DecideOp id
  }

decide :: forall name a . Eff (NamedEff name (DecideEff a)) a
decide = decideOp $ unLabelOps $
  captureOps @(NamedEff name (DecideEff a))
