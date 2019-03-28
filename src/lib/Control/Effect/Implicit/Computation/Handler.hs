
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

type BaseHandler handler eff = Handler NoEff handler eff

type GenericHandler ops handler
  = forall eff . (Effect eff) => Handler ops handler eff

mkHandler
  :: forall ops handler eff1 .
  ( EffOps ops
  , EffOps handler
  , Effect eff1
  )
  => (forall eff2 .
       (EffConstraint ops eff2)
       => LiftEff eff1 eff2
       -> Operation handler eff2
     )
  -> Handler ops handler eff1
mkHandler comp = Computation $
  \ lift12 ops -> withOps ops $ comp lift12

baseHandler
  :: forall handler eff .
  (EffOps handler, Effect eff)
  => Operation handler eff
  -> BaseHandler handler eff
baseHandler handler = Computation $
  \ lift12 _ -> applyEffmap lift12 handler

genericHandler
  :: forall ops handler .
  (EffOps ops, EffOps handler)
  => (forall eff .
      (EffConstraint ops eff)
      => Operation handler eff)
  -> GenericHandler ops handler
genericHandler handler = Computation $
  \ _ ops -> withOps ops $ handler

bindExactHandler
  :: forall ops handler eff1 comp .
  ( EffOps ops
  , EffOps handler
  , Effect eff1
  )
  => Handler ops handler eff1
  -> Computation (handler ∪ ops) comp eff1
  -> Computation ops comp eff1
bindExactHandler handler1 comp1
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
{-# INLINE bindExactHandler #-}

composeExactHandlers
  :: forall ops handler1 handler2 eff1 .
  ( EffOps ops
  , EffOps handler1
  , EffOps handler2
  , Effect eff1
  )
  => Handler ops handler1 eff1
  -> Handler (handler1 ∪ ops) handler2 eff1
  -> Handler ops (handler1 ∪ handler2) eff1
composeExactHandlers handler1 handler2
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

withHandler
  :: forall ops handler eff r .
  ( EffOps ops
  , EffOps handler
  , EffConstraint ops eff
  )
  => Handler ops handler eff
  -> (OpsConstraint handler eff => r)
  -> r
withHandler handler comp =
  withOps (runComp handler idLift captureOps) comp
{-# INLINE withHandler #-}

castHandler
  :: forall ops1 ops2 handler eff .
  ( Effect eff
  , EffOps ops1
  , EffOps ops2
  )
  => ops1 ⊇ ops2
  -> Handler ops2 handler eff
  -> Handler ops1 handler eff
castHandler = castComputation

composeHandlersWithCast
  :: forall
    ops1 ops2 ops3
    handler1 handler2
    eff .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  , EffOps handler1
  , EffOps handler2
  , Effect eff
  )
  => ops3 ⊇ ops1
  -> (handler1 ∪ ops3) ⊇ ops2
  -> Handler ops1 handler1 eff
  -> Handler ops2 handler2 eff
  -> Handler ops3 (handler1 ∪ handler2) eff
composeHandlersWithCast cast31 cast32 handler1 handler2 =
  composeExactHandlers
    (castHandler cast31 handler1)
    (castHandler cast32 handler2)

bindHandlerWithCast
  :: forall ops3 ops1 ops2 handler eff r .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  , EffOps handler
  , Effect eff
  )
  => ops3 ⊇ ops1
  -> (handler ∪ ops3) ⊇ ops2
  -> Handler ops1 handler eff
  -> Computation ops2 r eff
  -> Computation ops3 r eff
bindHandlerWithCast cast31 cast32 handler comp =
  bindExactHandler
    (castHandler cast31 handler)
    (castComputation cast32 comp)
{-# INLINE bindHandlerWithCast #-}
