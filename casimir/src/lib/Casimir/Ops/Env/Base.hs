
module Casimir.Ops.Env.Base
where

import Data.QuasiParam.Tag

import Casimir.Base
import Casimir.Computation

data EnvEff e

data EnvOps e eff = EnvOps
  { askOp :: eff e }

instance EffOps (EnvEff e) where
  type Operation (EnvEff e) = EnvOps e

instance EffFunctor Lift (EnvOps e) where
  effmap (Lift lift) envOps = EnvOps
    { askOp = lift $ askOp envOps
    }
