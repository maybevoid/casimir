
module Control.Effect.Handler where

import Control.Monad.Free

import Control.Effect.Cast
import Control.Effect.Class
import Control.Effect.Union
import Control.Effect.Ops.NoOp
import Control.Effect.Computation

type BaseHandler handler eff = Handler NoOp handler eff eff

type GenericHandler ops handler = forall eff . Handler ops handler eff eff

type FreeHandler handler = BaseHandler handler (Free (FreeModel handler))

mkHandler
  :: forall ops handler outerEff innerEff .
  ( EffOps ops
  , EffOps handler
  , Effect outerEff
  , Effect innerEff
  )
  => LiftEff innerEff outerEff
  -> (forall eff .
      (Effect eff)
      => LiftEff outerEff eff
      -> (EffConstraint ops eff) => handler eff)
  -> Handler ops handler outerEff innerEff
mkHandler lifter comp = Handler lifter $ Computation comp

outerLiftHandler
  :: forall ops handler eff1 eff2 eff3 .
  ( EffOps ops
  , EffOps handler
  , Effect eff1
  , Effect eff2
  , Effect eff3
  )
  => LiftEff eff2 eff1
  -> Handler ops handler eff2 eff3
  -> Handler ops handler eff1 eff3
outerLiftHandler lift21 (Handler lift32 handler) =
  Handler (joinLift lift32 lift21) $
    liftComputation lift21 handler

innerLiftHandler
  :: forall ops handler eff1 eff2 eff3 .
  ( EffOps ops
  , EffOps handler
  , Effect eff1
  , Effect eff2
  , Effect eff3
  )
  => LiftEff eff3 eff2
  -> Handler ops handler eff1 eff2
  -> Handler ops handler eff1 eff3
innerLiftHandler lift32 (Handler lift21 handler) =
  Handler (joinLift lift32 lift21) handler

baseHandler
  :: forall handler eff .
  (EffOps handler, Effect eff)
  => handler eff
  -> BaseHandler handler eff
baseHandler handler = Handler idLift $
  Computation $ \lifter -> effmap lifter handler

genericHandler
  :: forall ops handler .
  (EffOps ops, EffOps handler)
  => (forall eff . (Effect eff, EffConstraint ops eff) => handler eff)
  -> GenericHandler ops handler
genericHandler handler = Handler idLift $ Computation comp1
  where
    comp1
      :: forall eff1 eff2 .
      (Effect eff1, Effect eff2)
      => LiftEff eff1 eff2
      -> ((EffConstraint ops eff2) => handler eff2)
    comp1 _ = handler

freeHandler
  :: forall handler .
  (EffOps handler)
  => FreeHandler handler
freeHandler = baseHandler $ freeModel id

withHandler
  :: forall ops handler eff1 eff2 r .
  ( EffOps ops
  , EffOps handler
  , Effect eff1
  , Effect eff2
  , EffConstraint ops eff1
  )
  => Handler ops handler eff1 eff2
  -> (EffConstraint handler eff1 => r)
  -> r
withHandler (Handler _ handler1) comp1 = comp2
  where
    comp2 = bindConstraint handler2 comp1

    handler2 :: handler eff1
    handler2 = runComp handler1 idLift

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
  (Handler lift32 handler2) =
    mkHandler (joinLift lift32 lift21) comp1
      where
        comp1 :: forall eff0 .
          (Effect eff0)
          => LiftEff eff1 eff0
          -> (( EffConstraint ops1 eff0
              , EffConstraint ops2 eff0
              )
              => Union handler1 handler2 eff0)
        comp1 lift10 = Union handler1' handler2'
          where
            handler1' :: handler1 eff0
            handler1' = runComp handler1 lift10

            handler2' :: handler2 eff0
            handler2' = bindConstraint handler1' $
              runComp handler2 $ joinLift lift21 lift10

composeHandlers
  :: forall ops1 ops2 ops3 ops4 handler1 handler2 eff1 eff2 eff3 .
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  , EffOps ops4
  , EffOps handler1
  , EffOps handler2
  , Effect eff1
  , Effect eff2
  , Effect eff3
  )
  => Handler ops1 handler1 eff1 eff2
  -> Handler ops2 handler2 eff2 eff3
  -> CastOps (Union handler1 ops4) ops2
  -> CastOps ops3 (Union ops1 ops4)
  -> Handler ops3 (Union handler1 handler2) eff1 eff3
composeHandlers handler1 handler2 cast1 cast2 =
  castHandler handler3 cast2
    where
      handler3 :: Handler (Union ops1 ops4) (Union handler1 handler2) eff1 eff3
      handler3 = composeExactHandlers handler1 handler2'

      handler2' :: Handler (Union handler1 ops4) handler2 eff2 eff3
      handler2' = castHandler handler2 cast1

applyExactHandler
  :: forall ops handler eff1 eff2 r .
  ( EffOps ops
  , EffOps handler
  , Effect eff1
  , Effect eff2
  , EffConstraint ops eff1
  )
  => Handler ops handler eff1 eff2
  -> Computation (Union ops handler) r eff2
  -> r eff1
applyExactHandler (Handler lift21 handler1) comp1 = comp2
  where
    comp2 :: r eff1
    comp2 = bindConstraint handler2 $ runComp comp1 lift21

    handler2 :: handler eff1
    handler2 = runComp handler1 idLift

applyHandler
  :: forall ops1 ops2 handler eff1 eff2 r .
  ( EffOps ops1
  , EffOps ops2
  , EffOps handler
  , Effect eff1
  , Effect eff2
  , EffConstraint ops1 eff1
  )
  => Handler ops1 handler eff1 eff2
  -> Computation ops2 r eff2
  -> CastOps (Union ops1 handler) ops2
  -> r eff1
applyHandler handler comp cast =
  applyExactHandler handler $ castComputation comp cast
