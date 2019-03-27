
module Control.Effect.Implicit.Computation.Handler
  ( BaseHandler
  , GenericHandler
  , mkHandler
  , withHandler
  , baseHandler
  , genericHandler
  , bindExactHandler
  , composeExactHandlers
  , castHandler
  , composeHandlersWithCast
  , bindHandlerWithCast
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation.Class
import Control.Effect.Implicit.Computation.Cast

type BaseHandler handler eff = Handler NoEff handler eff eff

type GenericHandler ops handler
  = forall eff . (Effect eff) => Handler ops handler eff eff

mkHandler
  :: forall ops handler eff1 eff2 .
  ( EffOps ops
  , EffOps handler
  , Effect eff1
  , Effect eff2
  )
  => LiftEff eff2 eff1
  -> (forall eff3 .
      (Effect eff3)
      => LiftEff eff1 eff3
      -> ((OpsConstraint ops eff3) => Operation handler eff3)
      )
  -> Handler ops handler eff1 eff2
mkHandler lift21 comp = Handler lift21 $ Computation $
  \ lift13 ops -> withOps ops $ comp lift13

baseHandler
  :: forall handler eff .
  (EffOps handler, Effect eff)
  => Operation handler eff
  -> BaseHandler handler eff
baseHandler handler = Handler idLift $ Computation $
  \ lift12 _ -> applyLift lift12 handler

genericHandler
  :: forall ops handler .
  (EffOps ops, EffOps handler)
  => (forall eff .
      (EffConstraint ops eff)
      => Operation handler eff)
  -> GenericHandler ops handler
genericHandler handler = Handler idLift $ Computation $
  \ _ ops -> withOps ops $ handler

bindExactHandler
  :: forall ops handler eff1 eff2 comp .
  ( EffOps ops
  , EffOps handler
  , Effect eff1
  , Effect eff2
  )
  => Handler ops handler eff1 eff2
  -> Computation (handler ∪ ops) comp eff2
  -> Computation ops comp eff1
bindExactHandler (Handler lift21 handler1) comp1
  = Computation comp2
   where
    comp2
      :: forall eff3 .
      (Effect eff3)
      => LiftEff eff1 eff3
      -> Operation ops eff3
      -> comp eff3
    comp2 lift13 ops
      = runComp comp1 (joinLift lift21 lift13) (handler2 ∪ ops)
       where
        handler2 :: Operation handler eff3
        handler2 = runComp handler1 lift13 ops
    {-# INLINE comp2 #-}
{-# INLINE bindExactHandler #-}

composeExactHandlers
  :: forall ops handler1 handler2 eff1 eff2 eff3 .
  ( EffOps ops
  , EffOps handler1
  , EffOps handler2
  , Effect eff1
  , Effect eff2
  , Effect eff3
  )
  => Handler ops handler1 eff1 eff2
  -> Handler (handler1 ∪ ops) handler2 eff2 eff3
  -> Handler ops (handler1 ∪ handler2) eff1 eff3
composeExactHandlers
  (Handler lift21 handler1)
  (Handler lift32 handler2)
  = Handler (joinLift lift32 lift21) $ Computation comp1
   where
    comp1
      :: forall eff4 .
      (Effect eff4)
      => LiftEff eff1 eff4
      -> Operation ops eff4
      -> Operation (handler1 ∪ handler2) eff4
    comp1 lift14 ops
      = handler3 ∪ handler4
       where
        handler3 :: Operation handler1 eff4
        handler3 = runComp handler1 lift14 ops

        handler4 :: Operation handler2 eff4
        handler4 = runComp handler2 (joinLift lift21 lift14) (handler3 ∪ ops)

withHandler
  :: forall ops handler eff1 eff2 r .
  ( Effect eff2
  , EffOps ops
  , EffOps handler
  , EffConstraint ops eff1
  )
  => Handler ops handler eff1 eff2
  -> (OpsConstraint handler eff1 => r)
  -> r
withHandler (Handler _ handler) comp =
  withOps (runComp handler idLift captureOps) comp
{-# INLINE withHandler #-}

castHandler
  :: forall ops1 ops2 handler eff1 eff2 .
  ( Effect eff1
  , Effect eff2
  , EffOps ops1
  , EffOps ops2
  )
  => ops1 ⊇ ops2
  -> Handler ops2 handler eff1 eff2
  -> Handler ops1 handler eff1 eff2
castHandler caster (Handler lift12 handler)
  = Handler lift12 $ castComputation caster handler

composeHandlersWithCast
  :: forall
    ops1 ops2 ops3
    handler1 handler2
    eff1 eff2 eff3 .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  , EffOps handler1
  , EffOps handler2
  , Effect eff1
  , Effect eff2
  , Effect eff3
  )
  => Handler ops1 handler1 eff1 eff2
  -> Handler ops2 handler2 eff2 eff3
  -> ops3 ⊇ ops1
  -> (handler1 ∪ ops3) ⊇ ops2
  -> Handler ops3 (handler1 ∪ handler2) eff1 eff3
composeHandlersWithCast handler1 handler2 cast31 cast32 =
  composeExactHandlers
    (castHandler cast31 handler1) $
    castHandler cast32 handler2

bindHandlerWithCast
  :: forall ops3 ops1 ops2 handler eff1 eff2 r .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  , EffOps handler
  , Effect eff1
  , Effect eff2
  )
  => Handler ops1 handler eff1 eff2
  -> Computation ops2 r eff2
  -> ops3 ⊇ ops1
  -> (handler ∪ ops3) ⊇ ops2
  -> Computation ops3 r eff1
bindHandlerWithCast handler comp cast31 cast32 =
  bindExactHandler
    (castHandler cast31 handler) $
    castComputation cast32 comp
{-# INLINE bindHandlerWithCast #-}
