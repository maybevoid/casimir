{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Effect.Class
  ( Effect
  , LiftEff (..)
  , Computation (..)
  , Handler (..)
  , FreeEff (..)
  , EffFunctor (..)
  , EffOps (..)
  , idLift
  , joinLift
  )
where

import GHC.Exts (Constraint)
import Control.Monad.Free (Free)
import Control.Natural (type (~>))

type Effect eff = Monad eff

data LiftEff eff1 eff2 = LiftEff {
  liftEff :: forall a. eff1 a -> eff2 a
}

data Computation ops comp eff = Computation {
  runComp :: forall eff' .
    (EffOps ops, Effect eff, Effect eff')
    => LiftEff eff eff'
    -> ((EffConstraint ops eff') => comp eff')
}

data Handler ops handler outerEff innerEff =
  Handler (LiftEff innerEff outerEff) (Computation ops handler outerEff)

class EffFunctor (f :: (* -> *) -> *) where
  effmap :: forall eff1 eff2 .
    (Effect eff1, Effect eff2)
    => LiftEff eff1 eff2
    -> f eff1
    -> f eff2

class FreeEff f where
  type family FreeModel f :: (* -> *)

  freeModel
    :: forall g .
    (Functor g)
    => (FreeModel f ~> g)
    -> f (Free g)

class
  ( EffFunctor f
  , FreeEff f
  , Functor (FreeModel f)
  )
  => EffOps (f :: (* -> *) -> *) where
    type family EffConstraint f (eff :: * -> *) :: Constraint

    bindConstraint :: forall eff r .
      (Effect eff)
      => f eff
      -> (EffConstraint f eff => r)
      -> r

idLift :: forall eff . LiftEff eff eff
idLift = LiftEff id

joinLift :: forall eff1 eff2 eff3 .
  LiftEff eff1 eff2
  -> LiftEff eff2 eff3
  -> LiftEff eff1 eff3
joinLift lift12 lift23 = LiftEff $ (liftEff lift23) . (liftEff lift12)
