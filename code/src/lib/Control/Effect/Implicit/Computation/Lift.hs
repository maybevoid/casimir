module Control.Effect.Implicit.Computation.Lift
where

import Data.Kind
import Control.Effect.Implicit.Base

class
  (EffLifter lift)
  => FreeLifter t lift eff1 eff2 where
    freeLifter :: lift eff1 eff2

class EffLifter lift where
  type family Liftable lift
    (ops :: Type) :: Constraint

  idLiftEff
    :: forall eff . (Effect eff) => lift eff eff

  applyLiftEff
    :: forall eff1 eff2 ops
     . ( Effect eff1
       , Effect eff2
       , EffOps ops
       , Liftable lift ops
       )
    => lift eff1 eff2
    -> Operation ops eff1
    -> Operation ops eff2

  joinLiftEff
    :: forall eff1 eff2 eff3
     . ( Effect eff1
       , Effect eff2
       , Effect eff3
       )
    => lift eff1 eff2
    -> lift eff2 eff3
    -> lift eff1 eff3

  -- Workaround as quantified constraints does not
  -- allow the derived constraints to be used automatically
  withUnionLifts
    :: forall ops1 ops2 r
     . (Liftable lift ops1, Liftable lift ops2)
    => (Liftable lift (ops1 ∪ ops2) => r)
    -> r

data HigherLiftEff
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)
  = HigherLiftEff
    { baseLiftEff :: forall x . eff1 x -> eff2 x
    , contraLiftEff :: ContraLift eff1 eff2
    }

instance EffLifter LiftEff where
  type Liftable LiftEff ops =
    ( EffOps ops, EffFunctor (Operation ops) )

  idLiftEff = idLift
  applyLiftEff = applyEffmap
  joinLiftEff = joinLift

  withUnionLifts cont = cont

instance EffLifter HigherLiftEff where
  type Liftable HigherLiftEff ops =
    ( EffOps ops, HigherEffFunctor (Operation ops) )

  idLiftEff = HigherLiftEff id identityContraLift

  applyLiftEff (HigherLiftEff lift contraLift) ops =
    invEffmap lift contraLift ops

  joinLiftEff
    (HigherLiftEff lift1 contraLift1)
    (HigherLiftEff lift2 contraLift2)
    = HigherLiftEff
      (lift2 . lift1)
      (joinContraLift contraLift1 contraLift2)

  withUnionLifts cont = cont
