
module Casimir.Computation.Handler
  ( bindOps
  , bindExactOpsHandler
  , composeExactOpsHandlers
  , composeOpsHandlers
  , bindOpsHandler
  )
where

import Casimir.Base
import Casimir.Computation.Cast
import Casimir.Computation.Computation

bindOps
  :: forall ops1 ops2 lift comp m
   . ( Monad m
     , ImplicitOps ops1
     , ImplicitOps ops2
     , LiftMonoid lift
     , EffFunctor lift (Operations' ops1)
     , EffFunctor lift (Operations' ops2)
     )
  => Operations' ops1 m
  -> Computation lift (ops1 ∪ ops2) comp m
  -> Computation lift ops2 comp m
bindOps ops1 comp = Computation $
  \lift ops2 ->
    runComp comp lift $
      (effmap lift ops1) ∪ ops2

bindExactOpsHandler
  :: forall ops lift handler m1 comp
   . ( ImplicitOps ops
     , ImplicitOps handler
     , Monad m1
     , LiftMonoid lift
     , EffFunctor lift (Operations' handler)
     )
  => OpsHandler lift ops handler m1
  -> Computation lift (handler ∪ ops) comp m1
  -> Computation lift ops comp m1
bindExactOpsHandler handler1 comp1
  = Computation comp2
   where
    comp2
      :: forall m2
       . (Monad m2)
      => lift m1 m2
      -> Operations' ops m2
      -> comp m2
    comp2 lift12 ops
      = runComp comp1 lift12 (handler2 ∪ ops)
       where
        handler2 :: Operations' handler m2
        handler2 = runComp handler1 lift12 ops
    {-# INLINE comp2 #-}
{-# INLINE bindExactOpsHandler #-}

composeExactOpsHandlers
  :: forall ops lift handler1 handler2 m1
   . ( ImplicitOps ops
     , ImplicitOps handler1
     , ImplicitOps handler2
     , Monad m1
     )
  => OpsHandler lift ops handler1 m1
  -> OpsHandler lift (handler1 ∪ ops) handler2 m1
  -> OpsHandler lift ops (handler1 ∪ handler2) m1
composeExactOpsHandlers handler1 handler2
  = Computation comp1
   where
    comp1
      :: forall m2
       . (Monad m2)
      => lift m1 m2
      -> Operations' ops m2
      -> Operations' (handler1 ∪ handler2) m2
    comp1 lift12 ops
      = handler3 ∪ handler4
       where
        handler3 :: Operations' handler1 m2
        handler3 = runComp handler1 lift12 ops

        handler4 :: Operations' handler2 m2
        handler4 = runComp handler2 lift12 (handler3 ∪ ops)

composeOpsHandlers
  :: forall ops1 ops2 ops3 lift handler1 handler2 m
   . ( ImplicitOps ops1
     , ImplicitOps ops2
     , ImplicitOps ops3
     , ImplicitOps handler1
     , ImplicitOps handler2
     , Monad m
     , ops3 ⊇ ops1
     , (handler1 ∪ ops3) ⊇ ops2
     )
  => OpsHandler lift ops1 handler1 m
  -> OpsHandler lift ops2 handler2 m
  -> OpsHandler lift ops3 (handler1 ∪ handler2) m
composeOpsHandlers handler1 handler2 =
  composeExactOpsHandlers
    (castComputation handler1)
    (castComputation handler2)

bindOpsHandler
  :: forall ops3 ops1 ops2 lift handler m r
   . ( ImplicitOps ops1
     , ImplicitOps ops2
     , ImplicitOps ops3
     , ops3 ⊇ ops1
     , (handler ∪ ops3) ⊇ ops2
     , ImplicitOps handler
     , Monad m
     , LiftMonoid lift
     , EffFunctor lift (Operations' handler)
     )
  => OpsHandler lift ops1 handler m
  -> Computation lift ops2 r m
  -> Computation lift ops3 r m
bindOpsHandler handler comp =
  bindExactOpsHandler
    (castComputation handler)
    (castComputation comp)
