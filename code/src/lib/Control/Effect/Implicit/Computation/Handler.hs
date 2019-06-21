
module Control.Effect.Implicit.Computation.Handler
  ( BaseOpsHandler
  , GenericOpsHandler
  , bindOps
  , opsHandlerComp
  , withOpsHandler
  , baseOpsHandler
  , genericOpsHandler
  , bindExactOpsHandler
  , composeExactOpsHandlers
  , castOpsHandler
  , composeOpsHandlers
  , composeOpsHandlersWithCast
  , bindOpsHandlerWithCast
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation.Cast
import Control.Effect.Implicit.Computation.Computation

type BaseOpsHandler handler eff = Computation NoEff (Operation handler) eff

type GenericOpsHandler ops handler =
  forall eff . (Effect eff)
  => Computation ops (Operation handler) eff

bindOps
  :: forall ops1 ops2 comp eff
   . ( Effect eff
     , ImplicitOps ops1
     , ImplicitOps ops2
     )
  => Operation ops1 eff
  -> Computation (ops1 ∪ ops2) comp eff
  -> Computation ops2 comp eff
bindOps ops1 comp = Computation $
  \lift ops2 ->
    runComp comp lift $
      applyEffmap lift ops1 ∪ ops2

opsHandlerComp
  :: forall ops handler eff1 .
  ( ImplicitOps ops
  , ImplicitOps handler
  , Effect eff1
  )
  => (forall eff2 .
       (EffConstraint ops eff2)
       => LiftEff eff1 eff2
       -> Operation handler eff2
     )
  -> OpsHandler ops handler eff1
opsHandlerComp comp = Computation $
  \ lift12 ops -> withOps ops $ comp lift12

baseOpsHandler
  :: forall handler eff
   . (ImplicitOps handler, Effect eff)
  => Operation handler eff
  -> BaseOpsHandler handler eff
baseOpsHandler handler = Computation $
  \ lift12 _ -> applyEffmap lift12 handler

genericOpsHandler
  :: forall ops handler .
  (ImplicitOps ops, ImplicitOps handler)
  => (forall eff .
      (EffConstraint ops eff)
      => Operation handler eff)
  -> GenericOpsHandler ops handler
genericOpsHandler handler = Computation $
  \ _ ops -> withOps ops handler

bindExactOpsHandler
  :: forall ops handler eff1 comp .
  ( ImplicitOps ops
  , ImplicitOps handler
  , Effect eff1
  )
  => OpsHandler ops handler eff1
  -> Computation (handler ∪ ops) comp eff1
  -> Computation ops comp eff1
bindExactOpsHandler handler1 comp1
  = Computation comp2
   where
    comp2
      :: forall eff2 .
      (Effect eff2)
      => LiftEff eff1 eff2
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
  :: forall ops handler1 handler2 eff1 .
  ( ImplicitOps ops
  , ImplicitOps handler1
  , ImplicitOps handler2
  , Effect eff1
  )
  => OpsHandler ops handler1 eff1
  -> OpsHandler (handler1 ∪ ops) handler2 eff1
  -> OpsHandler ops (handler1 ∪ handler2) eff1
composeExactOpsHandlers handler1 handler2
  = Computation comp1
   where
    comp1
      :: forall eff2 .
      (Effect eff2)
      => LiftEff eff1 eff2
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
  :: forall ops handler eff r .
  ( ImplicitOps ops
  , ImplicitOps handler
  , EffConstraint ops eff
  )
  => OpsHandler ops handler eff
  -> (OpsConstraint handler eff => r)
  -> r
withOpsHandler handler =
  withOps (runComp handler idLift captureOps)
{-# INLINE withOpsHandler #-}

castOpsHandler
  :: forall ops1 ops2 handler eff .
  ( Effect eff
  , ImplicitOps ops1
  , ImplicitOps ops2
  )
  => ops1 ⊇ ops2
  -> OpsHandler ops2 handler eff
  -> OpsHandler ops1 handler eff
castOpsHandler = castComputation

composeOpsHandlersWithCast
  :: forall
    ops1 ops2 ops3
    handler1 handler2
    eff .
  ( ImplicitOps ops1
  , ImplicitOps ops2
  , ImplicitOps ops3
  , ImplicitOps handler1
  , ImplicitOps handler2
  , Effect eff
  )
  => ops3 ⊇ ops1
  -> (handler1 ∪ ops3) ⊇ ops2
  -> OpsHandler ops1 handler1 eff
  -> OpsHandler ops2 handler2 eff
  -> OpsHandler ops3 (handler1 ∪ handler2) eff
composeOpsHandlersWithCast cast31 cast32 handler1 handler2 =
  composeExactOpsHandlers
    (castOpsHandler cast31 handler1)
    (castOpsHandler cast32 handler2)

composeOpsHandlers
  :: forall
    ops1 ops2 ops3
    handler1 handler2
    eff .
  ( ImplicitOps ops1
  , ImplicitOps ops2
  , ImplicitOps ops3
  , ImplicitOps handler1
  , ImplicitOps handler2
  , Effect eff
  , EntailOps ops3 ops1
  , EntailOps (handler1 ∪ ops3) ops2
  )
  => OpsHandler ops1 handler1 eff
  -> OpsHandler ops2 handler2 eff
  -> OpsHandler ops3 (handler1 ∪ handler2) eff
composeOpsHandlers = composeOpsHandlersWithCast
  @ops1 @ops2 @ops3
  (opsEntailment @ops3 @ops1)
  (opsEntailment @(handler1 ∪ ops3) @ops2)

bindOpsHandlerWithCast
  :: forall ops3 ops1 ops2 handler eff r .
  ( ImplicitOps ops1
  , ImplicitOps ops2
  , ImplicitOps ops3
  , ImplicitOps handler
  , Effect eff
  )
  => ops3 ⊇ ops1
  -> (handler ∪ ops3) ⊇ ops2
  -> OpsHandler ops1 handler eff
  -> Computation ops2 r eff
  -> Computation ops3 r eff
bindOpsHandlerWithCast cast31 cast32 handler comp =
  bindExactOpsHandler
    (castOpsHandler cast31 handler)
    (castComputation cast32 comp)
{-# INLINE bindOpsHandlerWithCast #-}
