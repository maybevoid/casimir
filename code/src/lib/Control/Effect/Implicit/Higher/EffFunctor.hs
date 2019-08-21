
module Control.Effect.Implicit.Higher.EffFunctor
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.ContraLift

class HigherEffFunctor hops where
  invEffmap
    :: forall eff1 eff2
      . ( Effect eff1
        , Effect eff2
        )
    => (forall x . eff1 x -> eff2 x)
    -> ContraLiftEff eff1 eff2
    -> hops eff1 eff1
    -> hops eff2 eff2
