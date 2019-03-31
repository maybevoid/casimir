
module Control.Effect.Implicit.Computation.Cast
  ( Cast (..)
  , OpsCast
  , type (⊇)
  , cast
  , runCast
  , castOps
  , extendCast
  , composeCast
  , castComputation
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation.Computation

data Cast p = p => Cast

type OpsCast ops1 ops2 =
  forall eff . (EffConstraint ops1 eff) => Cast (OpsConstraint ops2 eff)

infixl 6 ⊇
type ops1 ⊇ ops2 = OpsCast ops1 ops2

cast :: forall p . p => Cast p
cast = Cast

runCast
  :: forall eff ops1 ops2 r .
  ( EffConstraint ops1 eff )
  => ops1 ⊇ ops2
  -> (OpsConstraint ops2 eff => r)
  -> r
runCast caster res =
  case caster @eff of
    Cast -> res

castOps
  :: forall eff ops1 ops2 .
  ( Effect eff
  , ImplicitOps ops1
  , ImplicitOps ops2
  )
  => ops1 ⊇ ops2
  -> Operation ops1 eff
  -> Operation ops2 eff
castOps caster ops = withOps ops $
  runCast @eff @ops1 @ops2
    caster captureOps

extendCast
  :: forall ops1 ops2 ops3 .
  ( ImplicitOps ops1
  , ImplicitOps ops2
  , ImplicitOps ops3
  )
  => ops1 ⊇ ops2
  -> (ops1 ∪ ops3) ⊇ (ops2 ∪ ops3)
extendCast caster1 = caster2
 where
  caster2
    :: forall eff .
    (EffConstraint (ops1 ∪ ops3) eff)
    => Cast (OpsConstraint (ops2 ∪ ops3) eff)
  caster2 = case caster1 @eff of
    Cast -> Cast

composeCast
  :: forall ops1 ops2 ops3.
  ( ImplicitOps ops1
  , ImplicitOps ops2
  , ImplicitOps ops3
  )
  => ops1 ⊇ ops2
  -> ops2 ⊇ ops3
  -> ops1 ⊇ ops3
composeCast cast1 cast2 = cast3
  where
    cast3
      :: forall eff .
      (EffConstraint ops1 eff)
      => Cast (OpsConstraint ops3 eff)
    cast3 = runCast @eff @ops1 @ops2 cast1 $
      runCast @eff @ops2 @ops3 cast2 Cast

castComputation
  :: forall ops1 ops2 comp eff .
  ( Effect eff
  , ImplicitOps ops1
  , ImplicitOps ops2
  )
  => ops1 ⊇ ops2
  -> Computation ops2 comp eff
  -> Computation ops1 comp eff
castComputation caster comp = Computation $
  \ lift12 ops ->
    runComp comp lift12 $ castOps caster ops
