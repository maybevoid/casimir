
module Casimir.Ops.Decide.Default
where

import Casimir.Base
import Casimir.Free

import Casimir.Ops.Decide.Base as Base

type DecideEff s = NamedEff "DecideEff" (Base.DecideEff s)
