
module Casimir.Computation.Cast
  ( castComputation
  )
where

import Casimir.Base
import Casimir.Cast
import Casimir.Computation.Computation

castComputation
  :: forall ops1 ops2 lift comp m .
  ( Monad m
  , EntailOps ops1 ops2
  )
  => Computation lift ops2 comp m
  -> Computation lift ops1 comp m
castComputation comp = Computation $
  \ lift12 ops ->
    runComp comp lift12 $ castOps ops
