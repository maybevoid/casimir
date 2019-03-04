
module Control.Effect.Handler
  ( FlatHandler
  , BaseHandler
  , GenericHandler
  , FreeHandler
  , mkHandler
  , outerLiftHandler
  , innerLiftHandler
  , baseHandler
  , genericHandler
  , freeHandler
  , flattenHandler
  , withHandler
  , composeExactHandlers
  , composeHandlersWithCast
  , applyExactHandler
  , applyHandlerWithCast
  , bindExactHandler
  , bindHandlerWithCast
  )
where

import Control.Monad.Trans.Free (Free)

import Control.Effect.Computation (liftComputation)

import Control.Effect.Cast
  ( CastOps (..)
  , castHandler
  , castComputation
  )

import Control.Effect.Base
  ( NoEff
  , Effect
  , LiftEff
  , Union
  , EffOps (..)
  , FreeEff (..)
  , Handler (..)
  , UnionOps (..)
  , EffConstraint
  , EffFunctor (..)
  , Computation (..)
  , joinLift
  )

type FlatHandler ops handler eff = Handler ops handler eff eff

type BaseHandler handler eff = FlatHandler NoEff handler eff

type GenericHandler ops handler = forall eff . FlatHandler ops handler eff

type FreeHandler handler = BaseHandler handler (Free (CoOperation handler))

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
      -> (EffConstraint ops eff) => Operation handler eff)
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
  => Operation handler eff
  -> BaseHandler handler eff
baseHandler handler = Handler id $
  Computation $ \lifter -> effmap lifter handler

genericHandler
  :: forall ops handler .
  (EffOps ops, EffOps handler)
  => (forall eff .
      (Effect eff, EffConstraint ops eff)
      => Operation handler eff)
  -> GenericHandler ops handler
genericHandler handler = Handler id $ Computation comp1
  where
    comp1
      :: forall eff1 eff2 .
      (Effect eff1, Effect eff2)
      => LiftEff eff1 eff2
      -> ((EffConstraint ops eff2)
          => Operation handler eff2)
    comp1 _ = handler

freeHandler
  :: forall handler .
  (EffOps handler)
  => FreeHandler handler
freeHandler = baseHandler $ freeMonad id

flattenHandler
  :: forall ops handler eff1 eff2 .
  ( EffOps ops
  , EffOps handler
  , Effect eff1
  , Effect eff2
  )
  => Handler ops handler eff1 eff2
  -> FlatHandler ops handler eff1
flattenHandler (Handler _ handler) = Handler id handler

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

    handler2 :: Operation handler eff1
    handler2 = runComp handler1 id

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
              => UnionOps handler1 handler2 eff0)
        comp1 lift10 = UnionOps handler1' handler2'
          where
            handler1' :: Operation handler1 eff0
            handler1' = runComp handler1 lift10

            handler2' :: Operation handler2 eff0
            handler2' = bindConstraint handler1' $
              runComp handler2 $ joinLift lift21 lift10

composeHandlersWithCast
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
composeHandlersWithCast handler1 handler2 cast1 cast2 =
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
  -> Computation (Union handler ops) r eff2
  -> r eff1
applyExactHandler (Handler lift21 handler1) comp1 = comp2
  where
    comp2 :: r eff1
    comp2 = bindConstraint handler2 $ runComp comp1 lift21

    handler2 :: Operation handler eff1
    handler2 = runComp handler1 id

applyHandlerWithCast
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
  -> CastOps (Union handler ops1) ops2
  -> r eff1
applyHandlerWithCast handler comp cast =
  applyExactHandler handler $ castComputation comp cast

bindExactHandler
  :: forall ops handler eff1 eff2 r .
  ( EffOps ops
  , EffOps handler
  , Effect eff1
  , Effect eff2
  )
  => Handler ops handler eff1 eff2
  -> Computation (Union handler ops) r eff2
  -> Computation ops r eff1
bindExactHandler handler comp1 = Computation comp2
  where
    comp2
      :: forall eff0 .
      (Effect eff0)
      => LiftEff eff1 eff0
      -> (EffConstraint ops eff0 => r eff0)
    comp2 lift10 = applyExactHandler (outerLiftHandler lift10 handler) comp1

bindHandlerWithCast
  :: forall ops1 ops2 handler eff1 eff2 r .
  ( EffOps ops1
  , EffOps ops2
  , EffOps handler
  , Effect eff1
  , Effect eff2
  )
  => Handler ops1 handler eff1 eff2
  -> Computation ops2 r eff2
  -> CastOps (Union handler ops1) ops2
  -> Computation ops1 r eff1
bindHandlerWithCast handler comp cast =
  bindExactHandler handler $ castComputation comp cast
