
module Control.Effect.Implicit.Base.EffFunctor
  ( EffFunctor (..)
  )
where

import Data.Kind

import Control.Effect.Implicit.Base.Effect

-- | An 'EffFunctor' @comp@ is parameterized by an 'Effect' @eff1@
-- and can be lifted to another 'Effect' @eff2@ by providing a natural
-- transformation @eff1 ~> eff2@ to 'effmap'. A simple example for @comp@
-- is 'Control.Effect.Implicit.Computation.Return'.
class EffFunctor (comp :: (Type -> Type) -> Type) where

  -- | Lift a computation @comp@ from 'Effect' @eff1@ to @eff2@
  -- through the given natural transformation.
  effmap
    :: forall eff1 eff2
     . (Effect eff1, Effect eff2)
    => (forall x . eff1 x -> eff2 x)
    -> comp eff1
    -> comp eff2
