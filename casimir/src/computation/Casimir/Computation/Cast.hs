
module Casimir.Computation.Cast
  ( castComputation
  , castComputationWithDict
  )
where

import Casimir.Base
import Casimir.Computation.Computation

castComputation
  :: forall eff1 eff2 lift comp m
   . ( CastOps eff1 eff2
     )
  => Computation lift eff2 comp m
  -> Computation lift eff1 comp m
castComputation comp = Computation $
  \ lift12 ops ->
    runComp comp lift12 $ castOps ops

castComputationWithDict
  :: forall eff1 eff2 lift comp m
   . (ImplicitOps eff1, ImplicitOps eff2)
  => CastDict eff1 eff2
  -> Computation lift eff2 comp m
  -> Computation lift eff1 comp m
castComputationWithDict dict comp1 = Computation comp2
 where
  comp2
    :: forall m2
         . ( Effects eff1
           , Monad m2
           )
        => lift m m2
        -> Operations' eff1 m2
        -> comp m2
  comp2 lift12 ops =
    runComp comp1 lift12 $ castOpsWithDict dict ops
