
module Control.Effect.Handler where

import Control.Effect.Class
import Control.Effect.Ops.NoOp
import Control.Effect.Union
import Control.Effect.Computation

type BaseHandler handler eff = Handler NoOp handler eff eff

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
  :: forall ops handler eff1.
  (EffOps ops, EffOps handler, Effect eff1)
  => (forall eff . (Effect eff, EffConstraint ops eff) => handler eff)
  -> Handler ops handler eff1 eff1
genericHandler handler = Handler idLift $ Computation comp1
  where
    comp1
      :: forall eff2 .
      (Effect eff2)
      => LiftEff eff1 eff2
      -> ((EffConstraint ops eff2) => handler eff2)
    comp1 _ = handler

composeHandlers
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
composeHandlers
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

withHandler
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
withHandler (Handler lift21 handler1) comp1 = comp2
    where
      handler2 :: handler eff1
      handler2 = runComp handler1 idLift

      comp2 :: (EffConstraint ops eff1) => r eff1
      comp2 = bindConstraint handler2 $ runComp comp1 lift21