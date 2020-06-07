
module Casimir.Base.Lift
  ( type (~>)
  , IdLift (..)
  , Lift (..)
  , LiftMonoid (..)
  , MaybeLift (..)
  , FreeLift (..)
  , HigherLift (..)
  , LiftFunctor (..)
  )
where

import Data.Kind
import Data.Functor.Identity

import Casimir.Base.ContraLift

type eff1 ~> eff2 = forall x . eff1 x -> eff2 x

class LiftFunctor
  (lift1 :: (Type -> Type) -> (Type -> Type) -> Type)
  (lift2 :: (Type -> Type) -> (Type -> Type) -> Type)
  where
    transformLift
      :: forall eff1 eff2
       . (Monad eff1, Monad eff2)
      => lift1 eff1 eff2
      -> lift2 eff1 eff2

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

data MaybeLift
  (lift :: (Type -> Type) -> (Type -> Type) -> Type)
  eff1 eff2
  where
    NoLift :: MaybeLift lift eff eff
    JustLift :: lift eff1 eff2 -> MaybeLift lift eff1 eff2

class
  (LiftMonoid lift)
  => FreeLift t lift eff1 eff2 where
    freeLift :: lift eff1 eff2

class LiftMonoid lift where
  idLift
    :: forall eff . (Monad eff) => lift eff eff

  joinLift
    :: forall eff1 eff2 eff3
     . ( Monad eff1
       , Monad eff2
       , Monad eff3
       )
    => lift eff1 eff2
    -> lift eff2 eff3
    -> lift eff1 eff3

instance LiftMonoid IdLift where
  idLift = IdLift
  joinLift IdLift IdLift = IdLift

instance LiftMonoid Lift where
  idLift = Lift id
  joinLift (Lift lift1) (Lift lift2) =
    Lift $ lift2 . lift1

instance LiftMonoid HigherLift where
  idLift = HigherLift id identityContraLift

  joinLift
    (HigherLift lift1 contraLift1)
    (HigherLift lift2 contraLift2)
    = HigherLift
      (lift2 . lift1)
      (joinContraLift contraLift1 contraLift2)

instance
  (LiftMonoid lift)
  => LiftMonoid (MaybeLift lift) where
    idLift = NoLift

    joinLift NoLift lift = lift
    joinLift lift NoLift = lift
    joinLift (JustLift lift1) (JustLift lift2) =
      JustLift $ joinLift lift1 lift2

instance LiftFunctor IdLift Lift where
  transformLift IdLift = Lift id

instance LiftFunctor IdLift HigherLift where
  transformLift
    :: forall eff1 eff2
      . (Monad eff1, Monad eff2)
    => IdLift eff1 eff2
    -> HigherLift eff1 eff2
  transformLift IdLift = HigherLift id $
    ContraLift contraLift
   where
    contraLift
      :: forall a
       . ((forall x . eff1 x -> eff1 (Identity x))
          -> eff1 (Identity a))
      -> eff1 a
    contraLift cont =
      fmap runIdentity $ cont $ fmap Identity

instance
  LiftFunctor IdLift (MaybeLift lift) where
    transformLift
      :: forall eff1 eff2
       . IdLift eff1 eff2
      -> MaybeLift lift eff1 eff2
    transformLift IdLift = NoLift

instance {-# OVERLAPPABLE #-}
  LiftFunctor lift (MaybeLift lift) where
    transformLift
      :: forall eff1 eff2
       . lift eff1 eff2
      -> MaybeLift lift eff1 eff2
    transformLift = JustLift

instance
  LiftFunctor HigherLift Lift where
    transformLift
      :: forall eff1 eff2
       . HigherLift eff1 eff2
      -> Lift eff1 eff2
    transformLift (HigherLift lift _) = Lift lift
