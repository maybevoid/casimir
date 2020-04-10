
module Casimir.Base.Lift
  ( IdLift (..)
  , Lift (..)
  , LiftOps (..)
  , FreeLift (..)
  , HigherLift (..)
  )
where

import Data.Kind

import Casimir.Base.Union
import Casimir.Base.Effect
import Casimir.Base.EffOps
import Casimir.Base.ContraLift
import Casimir.Base.EffFunctor

data IdLift
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)
  where
    IdLift :: IdLift eff eff

newtype Lift eff1 eff2 = Lift
  { runLift :: eff1 ~> eff2 }

data HigherLift
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)
  = HigherLift
    { hlBaseLift :: eff1 ~> eff2
    , hlContraLift :: ContraLift eff1 eff2
    }

class
  (LiftOps lift)
  => FreeLift t lift eff1 eff2 where
    freeLift :: lift eff1 eff2

class LiftOps lift where
  type family Liftable lift
    (ops :: Type) :: Constraint

  idLift
    :: forall eff . (Effect eff) => lift eff eff

  applyLift
    :: forall eff1 eff2 ops
     . ( Effect eff1
       , Effect eff2
       , EffOps ops
       , Liftable lift ops
       )
    => lift eff1 eff2
    -> Operation ops eff1
    -> Operation ops eff2

  joinLift
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

instance LiftOps IdLift where
  type Liftable IdLift ops = ()

  idLift = IdLift
  applyLift IdLift ops = ops
  joinLift IdLift IdLift = IdLift
  withUnionLifts cont = cont

instance LiftOps Lift where
  type Liftable Lift ops =
    ( EffOps ops, EffFunctor (Operation ops) )

  idLift = Lift id
  applyLift (Lift lift) = effmap lift
  joinLift (Lift lift1) (Lift lift2) = Lift $ lift2 . lift1

  withUnionLifts cont = cont

instance LiftOps HigherLift where
  type Liftable HigherLift ops =
    ( EffOps ops, HigherEffFunctor (Operation ops) )

  idLift = HigherLift id identityContraLift

  applyLift (HigherLift lift contraLift) ops =
    invEffmap lift contraLift ops

  joinLift
    (HigherLift lift1 contraLift1)
    (HigherLift lift2 contraLift2)
    = HigherLift
      (lift2 . lift1)
      (joinContraLift contraLift1 contraLift2)

  withUnionLifts cont = cont
