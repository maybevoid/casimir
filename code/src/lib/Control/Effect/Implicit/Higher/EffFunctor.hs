{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Higher.EffFunctor
  ( HigherEffFunctor (..)
  )
where

import Control.Effect.Implicit.Base
  ( EffFunctor (..)
  , ContraLift (..)
  )

import Control.Effect.Implicit.Higher.Base
import qualified Control.Effect.Implicit.Base as Base

class HigherEffFunctor ops where
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
  (EffFunctor ops)
  => HigherEffFunctor (HigherOps ops) where
    invEffmap lifter _ (HigherOps ops) =
      HigherOps $ Base.effmap lifter ops

instance
  (HigherEffFunctor ops)
  => Base.HigherEffFunctor (LowerOps ops) where
    invEffmap lift contraLift (LowerOps ops) =
      LowerOps $ invEffmap lift contraLift ops
