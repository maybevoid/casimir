
module Control.Effect.Implicit.Ops.Decide
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Free

data DecideOps s eff = DecideOps {
  decideOp :: eff s
}

data DecideCoOp s a = DecideOp (s -> a)
  deriving (Functor)

instance EffCoOp (DecideOps s) where
  type CoOperation (DecideOps s) = DecideCoOp s

instance EffFunctor (DecideOps s) where
  effmap lifter ops = DecideOps {
    decideOp = lifter $ decideOp ops
  }

instance FreeOps (DecideOps s) where
  mkFreeOps liftCoOp = DecideOps {
    decideOp = liftCoOp $ DecideOp id
  }

instance ImplicitOps (DecideOps s) where
  type OpsConstraint (DecideOps s) eff =
    (?_Control_Effect_Implicit_Ops_Decide_decideOps :: DecideOps s eff)

  withOps decideOps comp =
    let
      ?_Control_Effect_Implicit_Ops_Decide_decideOps =
        decideOps in comp

  captureOps =
    ?_Control_Effect_Implicit_Ops_Decide_decideOps

decide :: forall a . Eff (DecideOps a) a
decide = decideOp captureOps
