
module Control.Effect.Handler where

import Control.Effect.Class
import Control.Effect.Empty
import Control.Effect.Union
import Control.Effect.Computation

baseHandler
  :: forall handler eff .
  (EffRow handler, Effect eff)
  => handler eff
  -> Computation EmptyRow handler eff
baseHandler handler = Computation $ \lifter -> effmap lifter handler

handlerComputation
  :: forall ops handler eff1.
  (EffRow ops, EffRow handler, Effect eff1)
  => (forall eff . (Effect eff, EffConstraint ops eff) => handler eff)
  -> Computation ops handler eff1
handlerComputation handler = Computation comp1
  where
    comp1
      :: forall eff2 .
      (Effect eff2)
      => LiftEff eff1 eff2
      -> ((EffConstraint ops eff2) => handler eff2)
    comp1 _ = handler

composeHandlers
  :: forall eff1 ops1 ops2 handler1 handler2 .
  ( Effect eff1
  , EffRow ops1
  , EffRow ops2
  , EffRow handler1
  , EffRow handler2
  )
  => Computation ops1 handler1 eff1
  -> Computation (Union handler1 ops2) handler2 eff1
  -> Computation (Union ops1 ops2) (Union handler1 handler2) eff1
composeHandlers handler1 handler2 = Computation comp1
  where
    comp1 :: forall eff2 .
      (Effect eff2)
      => LiftEff eff1 eff2
      -> (( EffConstraint ops1 eff2
          , EffConstraint ops2 eff2
          )
          => Union handler1 handler2 eff2)
    comp1 lift12 = Union handler1' handler2'
      where
        handler1' :: handler1 eff2
        handler1' = runComp handler1 lift12

        handler2' :: handler2 eff2
        handler2' = bindConstraint handler1' $ runComp handler2 lift12

stackHandlers
  :: forall eff1 eff2 ops1 ops2 handler1 handler2 .
  ( Effect eff1
  , Effect eff2
  , EffRow ops1
  , EffRow ops2
  , EffRow handler1
  , EffRow handler2
  )
  => Computation ops1 handler1 eff1
  -> Computation (Union handler1 ops2) handler2 eff2
  -> LiftEff eff2 eff1
  -> Computation (Union ops1 ops2) (Union handler1 handler2) eff1
stackHandlers handler1 handler2 lift21 =
  composeHandlers handler1 $ liftComputation lift21 handler2