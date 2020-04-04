{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Implicit.MonadOps
where

import Data.Kind

import Control.Effect.Implicit.Base

data UseBase (m :: Type -> Type)

class
  ( EffOps (SupportedOps t) )
  => HasOps t where
    type family SupportedOps t :: Type

class
  ( HasOps t
  , Monad (OpsMonad t)
  )
  => MonadOps t where
    type family OpsMonad t :: Type -> Type

    monadOps
      :: Operation (SupportedOps t) (OpsMonad t)

class
  ( MonadOps t
  , Monad (BaseMonad t)
  )
  => LiftMonadOps t where
    type family BaseMonad t :: Type -> Type

    liftBase
      :: (BaseMonad t) ~> (OpsMonad t)

class
  ( LiftMonadOps t
  )
  => ContraLiftMonadOps t where
    contraLiftBase
      :: ContraLift (BaseMonad t) (OpsMonad t)

withMonadOps
  :: forall t r
   . ( MonadOps t
     , ImplicitOps (SupportedOps t)
     )
  => (EffConstraint (SupportedOps t) (OpsMonad t) => OpsMonad t r)
  -> OpsMonad t r
withMonadOps cont = withOps (monadOps @t) cont

class
  ( EffFunctor (Operation (SupportedOps t))
  ) => HasBaseOps t

instance
  ( EffFunctor (Operation (SupportedOps t))
  ) => HasBaseOps t

class
  ( HigherEffFunctor (Operation (SupportedOps t))
  ) => HasHigherOps t

instance
  ( HigherEffFunctor (Operation (SupportedOps t))
  ) => HasHigherOps t

instance HasOps (UseBase m) where
  type SupportedOps (UseBase m) = NoEff

instance
  (Monad m)
  => MonadOps (UseBase m) where
  type OpsMonad (UseBase m) = m

  monadOps = NoOp

instance
  (Monad m)
  => LiftMonadOps (UseBase m) where
    type BaseMonad (UseBase m) = m

    liftBase = id
