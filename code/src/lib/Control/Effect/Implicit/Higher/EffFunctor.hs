{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

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
  invEffmap lifter contraLifter =
    outerEffmap lifter . contraEffmap lifter contraLifter

  outerEffmap
    :: forall eff1 eff2 eff3
      . ( Effect eff1
        , Effect eff2
        , Effect eff3
        )
    => (forall x . eff2 x -> eff3 x)
    -> hops eff1 eff2
    -> hops eff1 eff3

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

instance {-# OVERLAPPABLE #-}
  (HigherEffFunctor hops, Effect eff)
  => EffFunctor (hops eff)
  where
    effmap = outerEffmap