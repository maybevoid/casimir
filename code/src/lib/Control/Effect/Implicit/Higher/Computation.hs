
module Control.Effect.Implicit.Higher.Computation
where

import Data.Kind
import Control.Effect.Implicit.Computation

data HigherLiftEff
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)
  = HigherLiftEff

type HigherComputation = Computation HigherLiftEff