
module Control.Effect.Implicit.Ops.Decide
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Free

data DecideEff s

data DecideOps s eff = DecideOps {
  decideOp :: eff s
}

data DecideCoOp s a = DecideOp (s -> a)
  deriving (Functor)

instance EffOps (DecideEff s) where
  type Operation (DecideEff s) = DecideOps s

instance EffCoOp (DecideEff s) where
  type CoOperation (DecideEff s) = DecideCoOp s

instance EffFunctor (DecideOps s) where
  effmap lifter ops = DecideOps {
    decideOp = lifter $ decideOp ops
  }

instance FreeOps (DecideEff s) where
  mkFreeOps liftCoOp = DecideOps {
    decideOp = liftCoOp $ DecideOp id
  }

instance ImplicitOps (DecideEff s) where
  type OpsConstraint (DecideEff s) eff =
    (?_Control_Effect_Implicit_Ops_Decide_decideOps :: DecideOps s eff)

  withOps decideOps comp =
    let
      ?_Control_Effect_Implicit_Ops_Decide_decideOps =
        decideOps in comp

  captureOps =
    ?_Control_Effect_Implicit_Ops_Decide_decideOps

decide :: forall a . Eff (DecideEff a) a
decide = decideOp captureOps
