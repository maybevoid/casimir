
module Control.Effect.Computation.Handler
where

import Control.Monad.Trans.Free (Free)

import Control.Effect.Base
import Control.Effect.Computation.Class

type FlatHandler ops handler eff = Handler ops handler eff eff

type BaseHandler handler eff = FlatHandler NoEff handler eff

type GenericHandler ops handler = forall eff . FlatHandler ops handler eff

type FreeHandler handler = BaseHandler handler (Free (CoOperation handler))

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
mkHandler lifter comp = Handler lifter $ Computation $
  \ liftEff ops -> bindConstraint ops $ comp liftEff

baseHandler
  :: forall handler eff .
  (EffOps handler, Effect eff)
  => Operation handler eff
  -> BaseHandler handler eff
baseHandler handler = Handler id $ Computation $
  \ liftEff _ -> effmap liftEff handler

genericHandler
  :: forall ops handler .
  (EffOps ops, EffOps handler)
  => (forall eff .
      (Effect eff, OpsConstraint ops eff)
      => Operation handler eff)
  -> GenericHandler ops handler
genericHandler handler = Handler id $ Computation $
  \ _ ops -> bindConstraint ops $ handler

freeHandler
  :: forall handler .
  (EffOps handler)
  => FreeHandler handler
freeHandler = baseHandler $ freeMonad id

bindExactHandler
  :: forall ops1 ops2 handler eff1 eff2 r .
  ( EffOps ops1
  , EffOps ops2
  , EffOps handler
  , Effect eff1
  , Effect eff2
  )
  => Handler ops1 handler eff1 eff2
  -> Computation (Union handler ops2) r eff2
  -> Computation (Union ops1 ops2) r eff1
bindExactHandler (Handler lift21 handler1) comp1
  = Computation comp2
   where
    comp2
      :: forall eff3 .
      (Effect eff3)
      => LiftEff eff1 eff3
      -> Operation (Union ops1 ops2) eff3
      -> r eff3
    comp2 liftEff (UnionOps ops1 ops2)
      = runComp comp1 (liftEff . lift21) (UnionOps handler2 ops2)
       where
        handler2 :: Operation handler eff3
        handler2 = runComp handler1 liftEff ops1

composeExactHandlers
  :: forall ops1 ops2 handler1 handler2 eff1 eff2 eff3 .
  ( EffOps ops1
  , EffOps ops2
  , EffOps handler1
  , EffOps handler2
  , Effect eff1
  , Effect eff2
  , Effect eff3
  )
  => Handler ops1 handler1 eff1 eff2
  -> Handler (Union handler1 ops2) handler2 eff2 eff3
  -> Handler (Union ops1 ops2) (Union handler1 handler2) eff1 eff3
composeExactHandlers
  (Handler lift21 handler1)
  (Handler lift32 handler2)
  = Handler (lift21 . lift32) $ Computation comp1
   where
    comp1
      :: forall eff4 .
      (Effect eff4)
      => LiftEff eff1 eff4
      -> Operation (Union ops1 ops2) eff4
      -> Operation (Union handler1 handler2) eff4
    comp1 lift14 (UnionOps ops1 ops2)
      = (UnionOps handler3 handler4)
       where
        handler3 :: Operation handler1 eff4
        handler3 = runComp handler1 lift14 ops1

        handler4 :: Operation handler2 eff4
        handler4 = runComp handler2 (lift14 . lift21) (UnionOps handler3 ops2)

withHandler
  :: forall handler eff1 eff2 r .
  ( Effect eff1
  , Effect eff2
  , EffOps handler
  )
  => Handler NoEff handler eff1 eff2
  -> (OpsConstraint handler eff1 => r)
  -> r
withHandler (Handler _ handler) comp =
  bindConstraint (runComp handler id NoOp) comp
