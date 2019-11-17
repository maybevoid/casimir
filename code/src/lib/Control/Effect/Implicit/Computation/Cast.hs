
module Control.Effect.Implicit.Computation.Cast
  ( castComputation
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Cast
import Control.Effect.Implicit.Computation.Computation

castComputation
  :: forall ops1 ops2 lift comp eff .
  ( Effect eff
  , ImplicitOps ops1
  , ImplicitOps ops2
  )
  => OpsCast ops1 ops2
  -> Computation lift ops2 comp eff
  -> Computation lift ops1 comp eff
castComputation caster comp = Computation $
  \ lift12 ops ->
    runComp comp lift12 $ castOps caster ops
