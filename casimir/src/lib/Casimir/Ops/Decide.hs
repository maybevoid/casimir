
module Casimir.Ops.Decide
where

import Casimir.Base
import Casimir.Free

data DecideEff s

data DecideOps s m = DecideOps {
  decideOp :: m s
}

data DecideCoOp s a = DecideOp (s -> a)
  deriving (Functor)

instance Effect (DecideEff s) where
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

instance ImplicitOps (DecideEff s) where
  type OpsConstraint (DecideEff s) m =
    (?_Control_Monad_Implicit_Ops_Decide_decideOps :: DecideOps s m)

  withOps decideOps comp =
    let
      ?_Control_Monad_Implicit_Ops_Decide_decideOps =
        decideOps in comp

  captureOps =
    ?_Control_Monad_Implicit_Ops_Decide_decideOps

decide :: forall a . Eff (DecideEff a) a
decide = decideOp captureOps
