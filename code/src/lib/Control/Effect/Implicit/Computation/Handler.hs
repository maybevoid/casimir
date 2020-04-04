
module Control.Effect.Implicit.Computation.Handler
  ( bindOps
  , opsHandlerComp
  , withOpsHandler
  , baseOpsHandler
  , genericOpsHandler
  , bindExactOpsHandler
  , composeExactOpsHandlers
  , castOpsHandler
  , composeOpsHandlers
  , composeOpsHandlersWithCast
  , bindOpsHandler
  , bindOpsHandlerWithCast
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Cast
import Control.Effect.Implicit.Computation.Cast
import Control.Effect.Implicit.Computation.Computation

bindOps
  :: forall ops1 ops2 lift comp eff
   . ( Effect eff
     , ImplicitOps ops1
     , ImplicitOps ops2
     , LiftOps lift
     , Liftable lift ops1
     , Liftable lift ops2
     )
  => Operation ops1 eff
  -> Computation lift (ops1 ∪ ops2) comp eff
  -> Computation lift ops2 comp eff
bindOps ops1 comp = Computation $
  \lift ops2 ->
    runComp comp lift $
      (applyLift lift ops1) ∪ ops2

opsHandlerComp
  :: forall ops lift handler eff1
   . ( ImplicitOps ops
     , ImplicitOps handler
     , Effect eff1
     )
  => (forall eff2 .
       (EffConstraint ops eff2)
       => lift eff1 eff2
       -> Operation handler eff2
     )
  -> OpsHandler lift ops handler eff1
opsHandlerComp comp = Computation $
  \ lift12 ops -> withOps ops $ comp lift12

baseOpsHandler
  :: forall lift handler eff
   . ( ImplicitOps handler
     , Effect eff
     , LiftOps lift
     , Liftable lift handler
     )
  => Operation handler eff
  -> OpsHandler lift NoEff handler eff
baseOpsHandler handler = Computation $
  \ lift12 _ -> applyLift lift12 handler

genericOpsHandler
  :: forall ops handler lift
   . ( ImplicitOps ops, ImplicitOps handler )
  => (forall eff
       . (EffConstraint ops eff)
      => Operation handler eff)
  -> (forall eff . OpsHandler lift ops handler eff)
genericOpsHandler handler = Computation $
  \ _ ops -> withOps ops handler

bindExactOpsHandler
  :: forall ops lift handler eff1 comp
   . ( ImplicitOps ops
     , ImplicitOps handler
     , Effect eff1
     , LiftOps lift
     , Liftable lift handler
     )
  => OpsHandler lift ops handler eff1
  -> Computation lift (handler ∪ ops) comp eff1
  -> Computation lift ops comp eff1
bindExactOpsHandler handler1 comp1
  = Computation comp2
   where
    comp2
      :: forall eff2
       . (Effect eff2)
      => lift eff1 eff2
      -> Operation ops eff2
      -> comp eff2
    comp2 lift12 ops
      = runComp comp1 lift12 (handler2 ∪ ops)
       where
        handler2 :: Operation handler eff2
        handler2 = runComp handler1 lift12 ops
    {-# INLINE comp2 #-}
{-# INLINE bindExactOpsHandler #-}

composeExactOpsHandlers
  :: forall ops lift handler1 handler2 eff1
   . ( ImplicitOps ops
     , ImplicitOps handler1
     , ImplicitOps handler2
     , Effect eff1
     )
  => OpsHandler lift ops handler1 eff1
  -> OpsHandler lift (handler1 ∪ ops) handler2 eff1
  -> OpsHandler lift ops (handler1 ∪ handler2) eff1
composeExactOpsHandlers handler1 handler2
  = Computation comp1
   where
    comp1
      :: forall eff2
       . (Effect eff2)
      => lift eff1 eff2
      -> Operation ops eff2
      -> Operation (handler1 ∪ handler2) eff2
    comp1 lift12 ops
      = handler3 ∪ handler4
       where
        handler3 :: Operation handler1 eff2
        handler3 = runComp handler1 lift12 ops

        handler4 :: Operation handler2 eff2
        handler4 = runComp handler2 lift12 (handler3 ∪ ops)

withOpsHandler
  :: forall ops lift handler eff r
   . ( ImplicitOps ops
     , ImplicitOps handler
     , EffConstraint ops eff
     , LiftOps lift
     , Liftable lift handler
     )
  => OpsHandler lift ops handler eff
  -> (OpsConstraint handler eff => r)
  -> r
withOpsHandler handler =
  withOps (runComp handler idLift captureOps)
{-# INLINE withOpsHandler #-}

castOpsHandler
  :: forall ops1 ops2 lift handler eff
   . ( Effect eff
     , ImplicitOps ops1
     , ImplicitOps ops2
     )
  => OpsCast ops1 ops2
  -> OpsHandler lift ops2 handler eff
  -> OpsHandler lift ops1 handler eff
castOpsHandler = castComputation

composeOpsHandlersWithCast
  :: forall ops1 ops2 ops3 lift handler1 handler2 eff
   . ( ImplicitOps ops1
     , ImplicitOps ops2
     , ImplicitOps ops3
     , ImplicitOps handler1
     , ImplicitOps handler2
     , Effect eff
     )
  => OpsCast ops3 ops1
  -> OpsCast (handler1 ∪ ops3) ops2
  -> OpsHandler lift ops1 handler1 eff
  -> OpsHandler lift ops2 handler2 eff
  -> OpsHandler lift ops3 (handler1 ∪ handler2) eff
composeOpsHandlersWithCast cast31 cast32 handler1 handler2 =
  composeExactOpsHandlers
    (castOpsHandler cast31 handler1)
    (castOpsHandler cast32 handler2)

composeOpsHandlers
  :: forall ops1 ops2 ops3 lift handler1 handler2 eff
   . ( ImplicitOps ops1
     , ImplicitOps ops2
     , ImplicitOps ops3
     , ImplicitOps handler1
     , ImplicitOps handler2
     , Effect eff
     , ops3 ⊇ ops1
     , (handler1 ∪ ops3) ⊇ ops2
     )
  => OpsHandler lift ops1 handler1 eff
  -> OpsHandler lift ops2 handler2 eff
  -> OpsHandler lift ops3 (handler1 ∪ handler2) eff
composeOpsHandlers = composeOpsHandlersWithCast
  @ops1 @ops2 @ops3
  (entailOps @ops3 @ops1)
  (entailOps @(handler1 ∪ ops3) @ops2)

bindOpsHandlerWithCast
  :: forall ops3 ops1 ops2 lift handler eff r
   . ( ImplicitOps ops1
     , ImplicitOps ops2
     , ImplicitOps ops3
     , ImplicitOps handler
     , Effect eff
     , LiftOps lift
     , Liftable lift handler
     )
  => OpsCast ops3 ops1
  -> OpsCast (handler ∪ ops3) ops2
  -> OpsHandler lift ops1 handler eff
  -> Computation lift ops2 r eff
  -> Computation lift ops3 r eff
bindOpsHandlerWithCast cast31 cast32 handler comp =
  bindExactOpsHandler
    (castOpsHandler cast31 handler)
    (castComputation cast32 comp)
{-# INLINE bindOpsHandlerWithCast #-}

bindOpsHandler
  :: forall ops3 ops1 ops2 lift handler eff r
   . ( ImplicitOps ops1
     , ImplicitOps ops2
     , ImplicitOps ops3
     , ops3 ⊇ ops1
     , (handler ∪ ops3) ⊇ ops2
     , ImplicitOps handler
     , Effect eff
     , LiftOps lift
     , Liftable lift handler
     )
  => OpsHandler lift ops1 handler eff
  -> Computation lift ops2 r eff
  -> Computation lift ops3 r eff
bindOpsHandler =
  bindOpsHandlerWithCast
    (entailOps @ops3 @ops1)
    (entailOps @(handler ∪ ops3) @ops2)
