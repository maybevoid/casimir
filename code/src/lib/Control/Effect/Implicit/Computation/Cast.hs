
module Control.Effect.Implicit.Computation.Cast
  ( castComputation
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Cast
import Control.Effect.Implicit.Computation.Computation

castComputation
  :: forall ops1 ops2 comp eff .
  ( Effect eff
  , BaseOps ops1
  , BaseOps ops2
  )
  => OpsCast ops1 ops2
  -> Computation ops2 comp eff
  -> Computation ops1 comp eff
castComputation caster comp = Computation $
  \ lift12 ops ->
    runComp comp lift12 $ castOps caster ops
