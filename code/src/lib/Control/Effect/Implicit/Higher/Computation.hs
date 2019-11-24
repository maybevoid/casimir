{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Implicit.Higher.Computation
where

import Data.Kind
import Control.Effect.Implicit.Base.Lift
import Control.Effect.Implicit.Base.Union
import Control.Effect.Implicit.Computation
import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.EffFunctor
import Control.Effect.Implicit.Higher.ContraLift

import qualified Control.Effect.Implicit.Base as Base

type HigherComputation = Computation HigherLiftEff
type HigherOpsHandler ops handler eff = OpsHandler HigherLiftEff ops handler eff

data HigherLiftEff
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)
  = HigherLiftEff
    { baseLiftEff :: forall x . eff1 x -> eff2 x
    , contraLiftEff :: ContraLift eff1 eff2
    }

class HigherEffFunctor' ops where
  invEffmap'
    :: forall eff1 eff2
      . ( Effect eff1
        , Effect eff2
        )
    => (forall x . eff1 x -> eff2 x)
    -> ContraLift eff1 eff2
    -> ops eff1
    -> ops eff2

instance
  {-# OVERLAPPING #-}
  (HigherEffFunctor ops)
  => HigherEffFunctor' (LowerOps ops) where
    invEffmap' lift contraLift (LowerOps ops) =
      LowerOps $ invEffmap lift contraLift ops

instance
  {-# OVERLAPPING #-}
  (HigherEffFunctor' ops1, HigherEffFunctor' ops2)
  => HigherEffFunctor' (UnionOps ops1 ops2) where
    invEffmap' lift contraLift (UnionOps ops1 ops2) =
      UnionOps
        (invEffmap' lift contraLift ops1)
        (invEffmap' lift contraLift ops2)

instance
  {-# OVERLAPPABLE #-}
  (Base.EffFunctor ops)
  => HigherEffFunctor' ops where
    invEffmap' lift _ ops = effmap lift ops

instance EffLifter HigherLiftEff where
  type Liftable HigherLiftEff ops =
    (HigherEffFunctor' (Base.Operation ops))

  idLiftEff = HigherLiftEff id identityContraLift

  applyLiftEff (HigherLiftEff lift contraLift) ops =
    invEffmap' lift contraLift ops

  joinLiftEff
    (HigherLiftEff lift1 contraLift1)
    (HigherLiftEff lift2 contraLift2)
    = HigherLiftEff
      (lift2 . lift1)
      (joinContraLift contraLift1 contraLift2)
