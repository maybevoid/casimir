
module Control.Effect.Computation.Class
where

import Control.Effect.Base

newtype Computation ops comp eff1 = Computation {
  runComp :: forall eff2 .
    ( EffOps ops
    , Effect eff1
    , Effect eff2
    )
    => eff1 ~> eff2
    -> Operation ops eff2
    -> comp eff2
}

data Handler ops handler eff1 eff2
  = Handler
    (eff2 ~> eff1)
    (Computation ops (Operation handler) eff1)
