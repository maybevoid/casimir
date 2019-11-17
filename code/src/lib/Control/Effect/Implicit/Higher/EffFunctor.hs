
module Control.Effect.Implicit.Higher.EffFunctor
  ( HigherEffFunctor (..)
  , EffFunctor (..)
  )
where

import Control.Effect.Implicit.Base (EffFunctor (..))
import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.ContraLift
import qualified Control.Effect.Implicit.Base as Base

class
  (forall eff . (Effect eff) => Base.EffFunctor (ops eff))
  => HigherEffFunctor ops where
    invEffmap
      :: forall eff1 eff2
       . ( Effect eff1
         , Effect eff2
         )
      => (forall x . eff1 x -> eff2 x)
      -> ContraLift eff1 eff2
      -> ops eff1 eff1
      -> ops eff2 eff2

instance
  (Base.EffFunctor ops)
  => HigherEffFunctor (HigherOps ops) where
    invEffmap lifter _ (HigherOps ops) =
      HigherOps $ Base.effmap lifter ops