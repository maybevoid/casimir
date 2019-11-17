
module Control.Effect.Implicit.Computation.Computation
  ( BaseComputation
  , Computation (..)
  , liftComputation
  , OpsHandler
  , BaseOpsHandler
  )
where

import Control.Effect.Implicit.Base

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

type OpsHandler lift ops handler eff = Computation lift ops (Operation handler) eff
type BaseOpsHandler ops handler eff = OpsHandler LiftEff ops handler eff

instance
  (BaseOps ops)
  => EffFunctor (BaseComputation ops comp) where
    effmap lifter =
      liftComputation (mkLiftEff lifter)

liftComputation
  :: forall lift ops comp eff1 eff2
   . ( BaseOps ops
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
