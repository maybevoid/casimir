
module Casimir.Computation.Handler
  -- ( bindOps
  -- , opsHandlerComp
  -- , withOpsHandler
  -- , baseOpsHandler
  -- , genericOpsHandler
  -- , bindExactOpsHandler
  -- , composeExactOpsHandlers
  -- , castOpsHandler
  -- , composeOpsHandlers
  -- , composeOpsHandlersWithCast
  -- , bindOpsHandler
  -- , bindOpsHandlerWithCast
  -- )
where

import Casimir.Base
import Casimir.Cast
import Casimir.Computation.Cast
import Casimir.Computation.Computation

bindOps
  :: forall ops1 ops2 lift comp m
   . ( Monad m
     , ImplicitOps ops1
     , ImplicitOps ops2
     , LiftMonoid lift
     , EffFunctor lift (Operation ops1)
     , EffFunctor lift (Operation ops2)
     )
  => Operation ops1 m
  -> Computation lift (ops1 ∪ ops2) comp m
  -> Computation lift ops2 comp m
bindOps ops1 comp = Computation $
  \lift ops2 ->
    runComp comp lift $
      (effmap lift ops1) ∪ ops2

opsHandlerComp
  :: forall ops lift handler m1
   . ( ImplicitOps ops
     , ImplicitOps handler
     , Monad m1
     )
  => (forall m2 .
       (EffConstraint ops m2)
       => lift m1 m2
       -> Operation handler m2
     )
  -> OpsHandler lift ops handler m1
opsHandlerComp comp = Computation $
  \ lift12 ops -> withOps ops $ comp lift12

baseOpsHandler
  :: forall lift handler m
   . ( ImplicitOps handler
     , Monad m
     , LiftMonoid lift
     , EffFunctor lift (Operation handler)
     )
  => Operation handler m
  -> OpsHandler lift NoEff handler m
baseOpsHandler handler = Computation $
  \ lift12 _ -> effmap lift12 handler

genericOpsHandler
  :: forall ops handler lift
   . ( ImplicitOps ops, ImplicitOps handler )
  => (forall m
       . (EffConstraint ops m)
      => Operation handler m)
  -> (forall m . OpsHandler lift ops handler m)
genericOpsHandler handler = Computation $
  \ _ ops -> withOps ops handler

bindExactOpsHandler
  :: forall ops lift handler m1 comp
   . ( ImplicitOps ops
     , ImplicitOps handler
     , Monad m1
     , LiftMonoid lift
     , EffFunctor lift (Operation handler)
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
      -> Operation ops m2
      -> comp m2
    comp2 lift12 ops
      = runComp comp1 lift12 (handler2 ∪ ops)
       where
        handler2 :: Operation handler m2
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
      -> Operation ops m2
      -> Operation (handler1 ∪ handler2) m2
    comp1 lift12 ops
      = handler3 ∪ handler4
       where
        handler3 :: Operation handler1 m2
        handler3 = runComp handler1 lift12 ops

        handler4 :: Operation handler2 m2
        handler4 = runComp handler2 lift12 (handler3 ∪ ops)

withOpsHandler
  :: forall ops lift handler m r
   . ( ImplicitOps ops
     , ImplicitOps handler
     , EffConstraint ops m
     , LiftMonoid lift
     , EffFunctor lift (Operation handler)
     )
  => OpsHandler lift ops handler m
  -> (OpsConstraint handler m => r)
  -> r
withOpsHandler handler =
  withOps (runComp handler idLift captureOps)
{-# INLINE withOpsHandler #-}

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

-- composeOpsHandlers
--   :: forall ops1 ops2 ops3 lift handler1 handler2 m
--    . ( ImplicitOps ops1
--      , ImplicitOps ops2
--      , ImplicitOps ops3
--      , ImplicitOps handler1
--      , ImplicitOps handler2
--      , Monad m
--      , ops3 ⊇ ops1
--      , (handler1 ∪ ops3) ⊇ ops2
--      )
--   => OpsHandler lift ops1 handler1 m
--   -> OpsHandler lift ops2 handler2 m
--   -> OpsHandler lift ops3 (handler1 ∪ handler2) m
-- composeOpsHandlers = composeOpsHandlersWithCast
--   @ops1 @ops2 @ops3
--   (entailOps @ops3 @ops1)
--   (entailOps @(handler1 ∪ ops3) @ops2)

-- bindOpsHandlerWithCast
--   :: forall ops3 ops1 ops2 lift handler m r
--    . ( ImplicitOps ops1
--      , ImplicitOps ops2
--      , ImplicitOps ops3
--      , ImplicitOps handler
--      , Monad m
--      , LiftMonoid lift
--      , EffFunctor lift (Operation handler)
--      )
--   => OpsCast ops3 ops1
--   -> OpsCast (handler ∪ ops3) ops2
--   -> OpsHandler lift ops1 handler m
--   -> Computation lift ops2 r m
--   -> Computation lift ops3 r m
-- bindOpsHandlerWithCast cast31 cast32 handler comp =
--   bindExactOpsHandler
--     (castOpsHandler cast31 handler)
--     (castComputation cast32 comp)
-- {-# INLINE bindOpsHandlerWithCast #-}

-- bindOpsHandler
--   :: forall ops3 ops1 ops2 lift handler m r
--    . ( ImplicitOps ops1
--      , ImplicitOps ops2
--      , ImplicitOps ops3
--      , ops3 ⊇ ops1
--      , (handler ∪ ops3) ⊇ ops2
--      , ImplicitOps handler
--      , Monad m
--      , LiftMonoid lift
--      , EffFunctor lift (Operation handler)
--      )
--   => OpsHandler lift ops1 handler m
--   -> Computation lift ops2 r m
--   -> Computation lift ops3 r m
-- bindOpsHandler =
--   bindOpsHandlerWithCast
--     (entailOps @ops3 @ops1)
--     (entailOps @(handler ∪ ops3) @ops2)
