module Casimir.Base.Lift where

import Data.Kind
import Data.Functor.Identity

import Casimir.Base.ContraLift

type m1 ~> m2 = forall x . m1 x -> m2 x

data IdLift
  (m1 :: Type -> Type)
  (m2 :: Type -> Type)
  where
    IdLift :: forall m . IdLift m m

newtype Lift m1 m2 = Lift
  { runLift :: m1 ~> m2 }

data MaybeLift
  (lift :: (Type -> Type) -> (Type -> Type) -> Type)
  m1 m2
  where
    NoLift
      :: forall lift m
       . MaybeLift lift m m

    JustLift
      :: forall lift m1 m2
       . lift m1 m2
      -> MaybeLift lift m1 m2

data HigherLift
  (m1 :: Type -> Type)
  (m2 :: Type -> Type)
  = HigherLift
    { baseLift :: m1 ~> m2
    , higherLift :: ContraLift m1 m2
    }

class LiftMonoid lift where
  idLift
    :: forall m . (Monad m) => lift m m

  joinLift
    :: forall m1 m2 m3
     . ( Monad m1
       , Monad m2
       , Monad m3
       )
    => lift m1 m2
    -> lift m2 m3
    -> lift m1 m3

class LiftFunctor
  (lift1 :: (Type -> Type) -> (Type -> Type) -> Type)
  (lift2 :: (Type -> Type) -> (Type -> Type) -> Type)
  where
    transformLift
      :: forall m1 m2
       . (Monad m1, Monad m2)
      => lift1 m1 m2
      -> lift2 m1 m2

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

instance {-# INCOHERENT #-}
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
    :: forall m1 m2
      . (Monad m1, Monad m2)
    => IdLift m1 m2
    -> HigherLift m1 m2
  transformLift IdLift = HigherLift id $
    ContraLift contraLift
   where
    contraLift
      :: forall a
       . ((forall x . m1 x -> m1 (Identity x))
          -> m1 (Identity a))
      -> m1 a
    contraLift cont =
      fmap runIdentity $ cont $ fmap Identity

instance {-# INCOHERENT #-}
  LiftFunctor IdLift (MaybeLift lift) where
    transformLift
      :: forall m1 m2
       . IdLift m1 m2
      -> MaybeLift lift m1 m2
    transformLift IdLift = NoLift

instance {-# INCOHERENT #-}
  LiftFunctor lift (MaybeLift lift) where
    transformLift
      :: forall m1 m2
       . lift m1 m2
      -> MaybeLift lift m1 m2
    transformLift = JustLift

instance
  LiftFunctor HigherLift Lift where
    transformLift
      :: forall m1 m2
       . HigherLift m1 m2
      -> Lift m1 m2
    transformLift (HigherLift lift _) = Lift lift
