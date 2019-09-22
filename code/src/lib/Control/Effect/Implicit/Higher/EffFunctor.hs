
module Control.Effect.Implicit.Higher.EffFunctor
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.ContraLift

class
  (forall eff . (Effect eff) => EffFunctor (hops eff))
  => HigherEffFunctor hops where
    invEffmap
      :: forall eff1 eff2
        . ( Effect eff1
          , Effect eff2
          )
      => (forall x . eff1 x -> eff2 x)
      -> ContraLiftEff eff1 eff2
      -> hops eff1 eff1
      -> hops eff2 eff2
    invEffmap lifter contraLifter =
      effmap lifter . contraEffmap lifter contraLifter

    contraEffmap
      :: forall eff1 eff2 eff3
        . ( Effect eff1
          , Effect eff2
          , Effect eff3
          )
      => (forall x . eff1 x -> eff2 x)
      -> ContraLiftEff eff1 eff2
      -> hops eff1 eff3
      -> hops eff2 eff3
