
module Control.Effect.Implicit.Cast.Cast
  ( Cast (..)
  , OpsCast
  , OpsCast'
  , cast
  , withCast
  , castOps
  , extendCast
  , composeCast
  )
where

import Control.Effect.Implicit.Base

data Cast p = p => Cast

type OpsCast' ops1 ops2 eff =
  (EffConstraint ops1 eff) => Cast (OpsConstraint ops2 eff)

type OpsCast ops1 ops2 =
  forall eff . OpsCast' ops1 ops2 eff

cast :: forall p . p => Cast p
cast = Cast

withCast
  :: forall eff ops1 ops2 r
   . ( EffConstraint ops1 eff )
  => OpsCast ops1 ops2
  -> (OpsConstraint ops2 eff => r)
  -> r
withCast caster res =
  case caster @eff of
    Cast -> res

castOps
  :: forall eff ops1 ops2 .
  ( Effect eff
  , ImplicitOps ops1
  , ImplicitOps ops2
  )
  => OpsCast ops1 ops2
  -> Operation ops1 eff
  -> Operation ops2 eff
castOps caster ops = withOps ops $
  withCast @eff @ops1 @ops2
    caster captureOps

extendCast
  :: forall ops1 ops2 ops3 .
  ( ImplicitOps ops1
  , ImplicitOps ops2
  , ImplicitOps ops3
  )
  => OpsCast ops1 ops2
  -> OpsCast (ops1 ∪ ops3) (ops2 ∪ ops3)
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
  => OpsCast ops1 ops2
  -> OpsCast ops2 ops3
  -> OpsCast ops1 ops3
composeCast cast1 cast2 = cast3
  where
    cast3
      :: forall eff .
      (EffConstraint ops1 eff)
      => Cast (OpsConstraint ops3 eff)
    cast3 = withCast @eff @ops1 @ops2 cast1 $
      withCast @eff @ops2 @ops3 cast2 Cast
