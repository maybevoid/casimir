{-# LANGUAGE PolyKinds #-}

module Casimir.Computation.Cast
  ( castComputation
  , castComputationWithDict
  )
where

import Casimir.Base
import Casimir.Computation.Computation

castComputation
  :: forall ops1 ops2 lift comp m
   . ( CastOps ops1 ops2
     )
  => Computation lift ops2 comp m
  -> Computation lift ops1 comp m
castComputation comp = Computation $
  \ lift12 ops ->
    runComp comp lift12 $ castOps ops

castComputationWithDict
  :: forall ops1 ops2 lift comp m
   . (Effects ops1, Effects ops2)
  => CastDict ops1 ops2
  -> Computation lift ops2 comp m
  -> Computation lift ops1 comp m
castComputationWithDict dict comp1 = Computation comp2
 where
  comp2
    :: forall m2
         . ( Effects ops1
           , Monad m2
           )
        => lift m m2
        -> ops1 m2
        -> comp m2
  comp2 lift12 ops =
    runComp comp1 lift12 $ castOpsWithDict dict ops
