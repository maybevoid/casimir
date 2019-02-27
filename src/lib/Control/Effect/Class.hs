{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}

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
import Control.Natural (type (~>))
import Control.Monad.Trans.Free (FreeT)

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

class EffFunctor (ops :: (* -> *) -> *) where
  effmap :: forall eff1 eff2 .
    (Effect eff1, Effect eff2)
    => LiftEff eff1 eff2
    -> ops eff1
    -> ops eff2

class FreeEff ops where
  type family FreeModel ops = (m :: (* -> *)) | m -> ops

  freeModel
    :: forall ops' eff .
    (Functor ops', Effect eff)
    => (FreeModel ops ~> ops')
    -> ops (FreeT ops' eff)

class
  ( EffFunctor ops
  , FreeEff ops
  , Functor (FreeModel ops)
  )
  => EffOps (ops :: (* -> *) -> *) where
    type family EffConstraint ops (eff :: * -> *) = (c :: Constraint) | c -> ops eff

    bindConstraint :: forall eff r .
      (Effect eff)
      => ops eff
      -> (EffConstraint ops eff => r)
      -> r

idLift :: forall eff . LiftEff eff eff
idLift = LiftEff id

joinLift :: forall eff1 eff2 eff3 .
  LiftEff eff1 eff2
  -> LiftEff eff2 eff3
  -> LiftEff eff1 eff3
joinLift lift12 lift23 = LiftEff $ (liftEff lift23) . (liftEff lift12)
