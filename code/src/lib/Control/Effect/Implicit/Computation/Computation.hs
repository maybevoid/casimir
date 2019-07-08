
module Control.Effect.Implicit.Computation.Computation
  ( Computation (..)
  , OpsHandler
  , liftComputation
  )
where

import Control.Effect.Implicit.Base

newtype Computation ops comp eff1 = Computation {
  runComp :: forall eff2 .
    ( BaseOps ops
    , Effect eff1
    , Effect eff2
    )
    => LiftEff eff1 eff2
    -> Operation ops eff2
    -> comp eff2
}

type OpsHandler ops handler eff
  = Computation ops (Operation handler) eff

instance
  (BaseOps ops)
  => EffFunctor (Computation ops comp) where
    effmap lifter =
      liftComputation (mkLiftEff lifter)

liftComputation
  :: forall ops comp eff1 eff2
   . ( BaseOps ops
     , Effect eff1
     , Effect eff2
     )
  => LiftEff eff1 eff2
  -> Computation ops comp eff1
  -> Computation ops comp eff2
liftComputation lift12 comp1 = Computation comp2
  where
    comp2 :: forall eff3 .
      ( Effect eff3
      )
      => LiftEff eff2 eff3
      -> Operation ops eff3
      -> comp eff3
    comp2 lift23 = runComp comp1 $ joinLift lift12 lift23
