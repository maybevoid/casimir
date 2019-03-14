
module Control.Effect.Computation.Handler
where

import Control.Effect.Base
import Control.Effect.Computation.Class

type FlatHandler ops handler eff = Handler ops handler eff eff

type BaseHandler handler eff = FlatHandler NoEff handler eff

type GenericHandler ops handler = forall eff . FlatHandler ops handler eff

type FreeHandler handler
  = forall eff . (Effect eff)
    => BaseHandler handler (FreeMonad handler eff)

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
freeHandler = baseHandler $ freeOps

bindExactHandler
  :: forall ops handler eff1 eff2 comp .
  ( EffOps ops
  , EffOps handler
  , Effect eff1
  , Effect eff2
  )
  => Handler ops handler eff1 eff2
  -> Computation (Union handler ops) comp eff2
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
    comp2 liftEff ops
      = runComp comp1 (liftEff . lift21) (UnionOps handler2 ops)
       where
        handler2 :: Operation handler eff3
        handler2 = runComp handler1 liftEff ops

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
  -> Handler (Union handler1 ops) handler2 eff2 eff3
  -> Handler ops (Union handler1 handler2) eff1 eff3
composeExactHandlers
  (Handler lift21 handler1)
  (Handler lift32 handler2)
  = Handler (lift21 . lift32) $ Computation comp1
   where
    comp1
      :: forall eff4 .
      (Effect eff4)
      => LiftEff eff1 eff4
      -> Operation ops eff4
      -> Operation (Union handler1 handler2) eff4
    comp1 lift14 ops
      = (UnionOps handler3 handler4)
       where
        handler3 :: Operation handler1 eff4
        handler3 = runComp handler1 lift14 ops

        handler4 :: Operation handler2 eff4
        handler4 = runComp handler2 (lift14 . lift21) (UnionOps handler3 ops)

withHandler
  :: forall ops handler eff1 eff2 r .
  ( Effect eff1
  , Effect eff2
  , EffOps ops
  , EffOps handler
  , OpsConstraint ops eff1
  )
  => Handler ops handler eff1 eff2
  -> (OpsConstraint handler eff1 => r)
  -> r
withHandler (Handler _ handler) comp =
  bindConstraint (runComp handler id captureOps) comp
