
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

class HigherEffFunctor hops where
  liftHigherOps
    :: forall eff1 eff2
      . ( Effect eff1
        , Effect eff2
        )
    => (forall x . eff1 x -> eff2 x)
    -> Weaver eff1 eff2
    -> hops eff1 eff1
    -> hops eff2 eff2
