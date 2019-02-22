
module Control.Effect.Computation where

import Control.Effect.Class
import Control.Effect.Union

liftComputation :: forall row eff1 eff2 r .
  (EffRow row, Monad eff1, Monad eff2)
  => LiftEff eff1 eff2
  -> Computation row eff1 r
  -> Computation row eff2 r
liftComputation lift12 comp1 = Computation comp2
  where
    comp2 :: forall eff3 .
      (Monad eff3)
      => LiftEff eff2 eff3
      -> (EffConstraint row eff3 => r eff3)
    comp2 lift23 = runComp comp1 $ joinLift lift12 lift23

withHandler
  :: forall eff1 row1 row2 r .
  ( Monad eff1
  , EffRow row1
  , EffRow row2
  )
  => row1 eff1
  -> Computation (UnionEffRow row1 row2) eff1 r
  -> Computation row2 eff1 r
withHandler ops comp1 = Computation comp2
  where
    comp2 :: forall eff2 .
      (Monad eff2)
      => LiftEff eff1 eff2
      -> (EffConstraint row2 eff2 => r eff2)
    comp2 lift12 = bindConstraint (effmap lift12 ops) comp3
      where
        comp3 :: (EffConstraint row1 eff2, EffConstraint row2 eff2) => r eff2
        comp3 = runComp comp1 lift12

applyHandler
  :: forall eff1 eff2 row1 row2 r .
  ( Monad eff1
  , Monad eff2
  , EffRow row1
  , EffRow row2
  )
  => row1 eff1
  -> LiftEff eff2 eff1
  -> Computation (UnionEffRow row1 row2) eff2 r
  -> Computation row2 eff1 r
applyHandler ops lift21 comp1 = withHandler ops comp2
  where
    comp2 :: Computation (UnionEffRow row1 row2) eff1 r
    comp2 = liftComputation lift21 comp1