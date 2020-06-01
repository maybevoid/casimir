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
    :: forall m1 m2
      . ( Monad m1
        , Monad m2
        )
    => lift m1 m2
    -> ops m1 m1
    -> ops m2 m2

instance {-# OVERLAPPABLE #-}
  (EffFunctor lift ops)
  => HigherEffFunctor lift (HigherOps ops) where
    higherEffmap lift (HigherOps ops) =
      HigherOps $ Base.mmap lift ops

instance {-# OVERLAPPABLE #-}
  (HigherEffFunctor HigherLift ops)
  => EffFunctor HigherLift (LowerOps ops) where
    mmap lift (LowerOps ops) =
      LowerOps $ higherEffmap lift ops
