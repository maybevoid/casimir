{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Effect.Class where

import GHC.Exts

data LiftEff eff1 eff2 = LiftEff {
  liftEff :: forall a. eff1 a -> eff2 a
}

joinLift :: forall eff1 eff2 eff3 .
  LiftEff eff1 eff2
  -> LiftEff eff2 eff3
  -> LiftEff eff1 eff3
joinLift lift12 lift23 = LiftEff $ (liftEff lift23) . (liftEff lift12)

idLift :: forall eff . LiftEff eff eff
idLift = LiftEff id

class EffFunctor (f :: (* -> *) -> *) where
  effmap :: forall eff1 eff2 .
    (Monad eff1, Monad eff2)
    => LiftEff eff1 eff2
    -> f eff1
    -> f eff2

class EffFunctor f => EffRow (f :: (* -> *) -> *) where
  type family EffConstraint f (eff :: * -> *) :: Constraint

  bindConstraint :: forall eff r .
    (Monad eff)
    => f eff
    -> (EffConstraint f eff => r)
    -> r

data Computation row eff r = Computation {
  runComp :: forall eff' .
    (EffRow row, Monad eff, Monad eff')
    => LiftEff eff eff'
    -> ((EffConstraint row eff') => r eff')
}
