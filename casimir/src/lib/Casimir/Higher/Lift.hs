module Casimir.Higher.Lift where

import Casimir.Base.ContraLift
import Casimir.Base.Lift (type (~>))
import Casimir.Higher.ArgKind

import Data.Kind
import Data.Functor.Identity

data IdLift
  (m1 :: MonadPair)
  (m2 :: MonadPair)
  where
    IdLift :: forall m . IdLift ('MonadPair m m) ('MonadPair m m)

data MaybeLift
  (lift :: MonadPair -> MonadPair -> Type)
  (m1 :: MonadPair)
  (m2 :: MonadPair)
  where
    NoLift
      :: forall lift m
       . MaybeLift lift ('MonadPair m m) ('MonadPair m m)

    JustLift
      :: forall lift m1 m2
       . lift m1 m2
      -> MaybeLift lift m1 m2

data BaseLift
  (m1 :: MonadPair)
  (m2 :: MonadPair)
  where
    BaseLift
      :: forall m1 m2
       . m1 ~> m2
      -> BaseLift ('MonadPair m1 m1) ('MonadPair m2 m2)

data HigherLift
  (m1 :: MonadPair)
  (m2 :: MonadPair)
  where
    HigherLift
      :: forall m1 m2
       . m1 ~> m2
      -> ContraLift m1 m2
      -> HigherLift ('MonadPair m1 m1) ('MonadPair m2 m2)

class LiftMonoid lift where
  idLift
    :: forall m
     . (Monad m)
    => lift ('MonadPair m m) ('MonadPair m m)

  joinLift
    :: forall m11 m12 m21 m22 m31 m32
     . ( Monad m11, Monad m12
       , Monad m21, Monad m22
       , Monad m31, Monad m32
       )
    => lift ('MonadPair m11 m12) ('MonadPair m21 m22)
    -> lift ('MonadPair m21 m22) ('MonadPair m31 m32)
    -> lift ('MonadPair m11 m12) ('MonadPair m31 m32)

class LiftFunctor
  (lift1 :: MonadPair -> MonadPair -> Type)
  (lift2 :: MonadPair -> MonadPair -> Type)
  where
    transformLift
      :: forall m11 m12 m21 m22
       . ( Monad m11
         , Monad m12
         , Monad m21
         , Monad m22
         )
      => lift1 ('MonadPair m11 m12) ('MonadPair m21 m22)
      -> lift2 ('MonadPair m11 m12) ('MonadPair m21 m22)

instance LiftFunctor IdLift BaseLift where
  transformLift IdLift = BaseLift id

instance LiftFunctor IdLift HigherLift where
  transformLift
      :: forall m11 m12 m21 m22
       . ( Monad m11
         , Monad m12
         , Monad m21
         , Monad m22
         )
      => IdLift ('MonadPair m11 m12) ('MonadPair m21 m22)
      -> HigherLift ('MonadPair m11 m12) ('MonadPair m21 m22)
  transformLift IdLift = HigherLift id $
    ContraLift contraLift
   where
    contraLift
      :: forall a
       . ((forall x . m11 x -> m11 (Identity x))
          -> m11 (Identity a))
      -> m11 a
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
  LiftFunctor HigherLift BaseLift where
    transformLift
      :: forall m1 m2
       . HigherLift m1 m2
      -> BaseLift m1 m2
    transformLift (HigherLift lift _) = BaseLift lift
