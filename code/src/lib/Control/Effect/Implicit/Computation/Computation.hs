
module Control.Effect.Implicit.Computation.Computation
  ( BaseComputation
  , Computation (..)
  , liftComputation
  , OpsHandler
  , BaseOpsHandler
  , strengthenComputation
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation.Lift

newtype Computation lift ops comp eff1 = Computation {
  runComp :: forall eff2 .
    ( EffOps ops
    , Effect eff1
    , Effect eff2
    )
    => lift eff1 eff2
    -> Operation ops eff2
    -> comp eff2
}

type BaseComputation = Computation LiftEff

type OpsHandler lift ops handler = Computation lift ops (Operation handler)
type BaseOpsHandler ops handler = OpsHandler LiftEff ops handler

instance
  (BaseOps ops)
  => EffFunctor (BaseComputation ops comp) where
    effmap lifter =
      liftComputation (mkLiftEff lifter)

liftComputation
  :: forall lift ops comp eff1 eff2
   . ( EffOps ops
     , Effect eff1
     , Effect eff2
     , EffLifter lift
     )
  => lift eff1 eff2
  -> Computation lift ops comp eff1
  -> Computation lift ops comp eff2
liftComputation lift12 comp1 = Computation comp2
  where
    comp2 :: forall eff3 .
      ( Effect eff3
      )
      => lift eff2 eff3
      -> Operation ops eff3
      -> comp eff3
    comp2 lift23 = runComp comp1 $ joinLiftEff lift12 lift23

strengthenComputation
  :: forall lift1 lift2 ops comp eff
   . ( EffOps ops
     , Effect eff
     , EffLifter lift1
     , EffLifter lift2
     )
  => (forall eff1 eff2
       . (Effect eff1, Effect eff2)
      => lift2 eff1 eff2
      -> lift1 eff1 eff2)
  -> Computation lift1 ops comp eff
  -> Computation lift2 ops comp eff
strengthenComputation strengthenLift comp1 =
  Computation comp2
 where
  comp2 :: forall eff2 .
    ( Effect eff2
    )
    => lift2 eff eff2
    -> Operation ops eff2
    -> comp eff2
  comp2 lift = runComp comp1 $ strengthenLift lift