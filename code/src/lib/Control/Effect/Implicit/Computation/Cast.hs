{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Implicit.Computation.Cast
  ( Cast (..)
  , OpsCast
  , OpsCast'
  , EntailOps (..)
  , EntailOps' (..)
  , type (⊇)
  , cast
  , withCast
  , castOps
  , extendCast
  , composeCast
  , castComputation
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation.Computation

data Cast p = p => Cast

type OpsCast' ops1 ops2 eff =
  (EffConstraint ops1 eff) => Cast (OpsConstraint ops2 eff)

type OpsCast ops1 ops2 =
  forall eff . OpsCast' ops1 ops2 eff

infixl 6 ⊇
type ops1 ⊇ ops2 = OpsCast ops1 ops2

cast :: forall p . p => Cast p
cast = Cast

class
  (Effect eff, ImplicitOps ops1, ImplicitOps ops2)
  => EntailOps' ops1 ops2 eff
  where
    entailOps' :: OpsCast' ops1 ops2 eff

instance
  ( Effect eff, ImplicitOps ops1, ImplicitOps ops2
  , (p ~ OpsConstraint ops1 eff, q ~ OpsConstraint ops2 eff)
  , (p => q)
  )
  => EntailOps' ops1 ops2 eff
  where
    entailOps' :: OpsCast' ops1 ops2 eff
    entailOps' = Cast

class
  (ImplicitOps ops1, ImplicitOps ops2)
  => EntailOps ops1 ops2
  where
    entailOps :: OpsCast ops1 ops2

instance
  ( ImplicitOps ops1, ImplicitOps ops2
  , forall eff . Effect eff => EntailOps' ops1 ops2 eff
  )
  => EntailOps ops1 ops2
  where
    entailOps :: forall eff . OpsCast' ops1 ops2 eff
    entailOps = entailOps' @ops1 @ops2 @eff

withCast
  :: forall eff ops1 ops2 r
   . ( EffConstraint ops1 eff )
  => ops1 ⊇ ops2
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
  => ops1 ⊇ ops2
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
    cast3 = withCast @eff @ops1 @ops2 cast1 $
      withCast @eff @ops2 @ops3 cast2 Cast

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
