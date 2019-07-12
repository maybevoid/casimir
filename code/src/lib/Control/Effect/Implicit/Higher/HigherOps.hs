
module Control.Effect.Implicit.Higher.HigherOps
where

import Data.Kind

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.Weaver

class
  HigherOps hops where
    type family HOperation hops
      = (operation :: (Type -> Type) -> (Type -> Type) -> Type)
      | operation -> hops

class HigherOpsFunctor hops where
  liftHigherOps
    :: forall eff1 eff2
      . ( Effect eff1
        , Effect eff2
        )
    => hops eff1 eff1
    -> Weaver eff1 eff2
    -> hops eff2 eff2
