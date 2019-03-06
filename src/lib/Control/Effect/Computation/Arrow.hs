
module Control.Effect.Computation.Arrow
where

import Control.Effect.Base
import Control.Effect.Computation.Class
import Control.Effect.Computation.Value

newtype Arrow handler a b eff = Arrow {
  applyComp ::
    Computation handler (Return a) eff
    -> eff b
}

type ArrowComputation ops handler a b eff
  = Computation ops (Arrow handler a b) eff

withArrow
  :: forall ops handler a b eff .
  ( Effect eff
  , EffOps ops
  , EffOps handler
  , OpsConstraint ops eff
  )
  => ArrowComputation ops handler a b eff
  -> (forall eff2 .
      (Effect eff2, OpsConstraint handler eff2)
      => eff2 a)
  -> eff b
withArrow func arg = applyComp
  (runComp func id captureOps) $
    genericComputation @handler arg

bindArrow
  :: forall ops handler a b eff1 .
  ( Effect eff1
  , EffOps ops
  , EffOps handler
  )
  => ArrowComputation ops handler a b eff1
  -> Computation (Union handler ops) (Return a) eff1
  -> Computation ops (Return b) eff1
bindArrow arrow1 comp1 = Computation comp2
 where
  comp2
    :: forall eff2 .
    (Effect eff2)
    => LiftEff eff1 eff2
    -> Operation ops eff2
    -> Return b eff2
  comp2 lift12 ops1 = Return $ applyComp arrow2 $ Computation comp3
   where
    arrow2 :: Arrow handler a b eff2
    arrow2 = runComp arrow1 lift12 ops1

    comp3 :: forall eff3 .
      (Effect eff3)
      => LiftEff eff2 eff3
      -> Operation handler eff3
      -> Return a eff3
    comp3 lift23 ops2 = runComp comp1 (lift23 . lift12)
      (UnionOps ops2 (effmap lift23 ops1))

composeArrows
  :: forall ops handler1 handler2 a b c eff1 .
  ( Effect eff1
  , EffOps ops
  , EffOps handler1
  , EffOps handler2
  )
  => ArrowComputation ops handler1 a b eff1
  -> ArrowComputation ops handler2 b c eff1
  -> ArrowComputation ops (Union handler1 handler2) a c eff1
composeArrows arrow1 arrow2 = Computation arrow3
 where
  arrow3
    :: forall eff2 .
    (Effect eff2)
    => LiftEff eff1 eff2
    -> Operation ops eff2
    -> Arrow (Union handler1 handler2) a c eff2
  arrow3 lift12 ops1 = Arrow arrow4
   where
    arrow4
      :: Computation (Union handler1 handler2) (Return a) eff2
      -> eff2 c
    arrow4 comp1 = applyComp arrow6 $ Computation comp2
     where
      comp2 :: forall eff3 .
        (Effect eff3)
        => LiftEff eff2 eff3
        -> Operation handler2 eff3
        -> Return b eff3
      comp2 lift23 ops2 =
        Return $ applyComp arrow5 $ Computation comp3
       where
        comp3
          :: forall eff4 .
          (Effect eff4)
          => LiftEff eff3 eff4
          -> Operation handler1 eff4
          -> Return a eff4
        comp3 lift34 ops3 = runComp comp1
          (lift34 . lift23)
          (UnionOps ops3 (effmap lift34 ops2))

        arrow5 :: Arrow handler1 a b eff3
        arrow5 = runComp arrow1 (lift23 . lift12) (effmap lift23 ops1)

    arrow6 :: Arrow handler2 b c eff2
    arrow6 = runComp arrow2 lift12 ops1

mkHandlerArrow
  :: forall ops handler eff1 eff2 .
  ( Effect eff1
  , Effect eff2
  , EffOps ops
  , EffOps handler
  )
  => Handler ops handler eff1 eff2
  -> (forall a . ArrowComputation ops handler a a eff1)
mkHandlerArrow (Handler _ handler1) = Computation comp1
 where
  comp1
    :: forall eff3 .
    (Effect eff3)
    => LiftEff eff1 eff3
    -> Operation ops eff3
    -> (forall a . Arrow handler a a eff3)
  comp1 lift13 ops1 = Arrow arrow1
   where
    arrow1 :: forall a .
      Computation handler (Return a) eff3
      -> eff3 a
    arrow1 comp2 = returnVal $ runComp comp2 id handler2

    handler2 :: Operation handler eff3
    handler2 = runComp handler1 lift13 ops1