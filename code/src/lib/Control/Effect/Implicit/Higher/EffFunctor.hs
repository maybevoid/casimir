
module Control.Effect.Implicit.Higher.EffFunctor
where

import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.ContraLift

class EffFunctor ops where
  invEffmap
    :: forall eff1 eff2
      . ( Effect eff1
        , Effect eff2
        )
    => (forall x . eff1 x -> eff2 x)
    -> ContraLift eff1 eff2
    -> ops eff1 eff1
    -> ops eff2 eff2
