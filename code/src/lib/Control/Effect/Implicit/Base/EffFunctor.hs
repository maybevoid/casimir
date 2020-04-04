{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Implicit.Base.EffFunctor
  ( EffFunctor (..)
  , HigherEffFunctor (..)
  )
where

import Data.Kind

import Control.Effect.Implicit.Base.Effect
import Control.Effect.Implicit.Base.ContraLift

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
    => eff1 ~> eff2
    -> comp eff1
    -> comp eff2

class HigherEffFunctor ops where
  invEffmap
    :: forall eff1 eff2
      . ( Effect eff1
        , Effect eff2
        )
    => eff1 ~> eff2
    -> ContraLift eff1 eff2
    -> ops eff1
    -> ops eff2

instance
  {-# OVERLAPPABLE #-}
  (EffFunctor ops)
  => HigherEffFunctor ops where
    invEffmap lifter _ ops =
      effmap lifter ops
