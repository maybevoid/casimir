{-# OPTIONS_GHC -fno-warn-orphans #-}

module Casimir.Higher.EffFunctor
  ( HigherEffFunctor (..)
  )
where

import Casimir.Base
  ( EffFunctor (..)
  , HigherLift
  )

import Casimir.Higher.Base
import qualified Casimir.Base as Base

class HigherEffFunctor lift ops where
  higherEffmap
    :: forall eff1 eff2
      . ( Monad eff1
        , Monad eff2
        )
    => lift eff1 eff2
    -> ops eff1 eff1
    -> ops eff2 eff2

instance {-# OVERLAPPABLE #-}
  (EffFunctor lift ops)
  => HigherEffFunctor lift (HigherOps ops) where
    higherEffmap lift (HigherOps ops) =
      HigherOps $ Base.effmap lift ops

instance {-# OVERLAPPABLE #-}
  (HigherEffFunctor HigherLift ops)
  => EffFunctor HigherLift (LowerOps ops) where
    effmap lift (LowerOps ops) =
      LowerOps $ higherEffmap lift ops
