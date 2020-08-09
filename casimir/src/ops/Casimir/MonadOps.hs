{-# LANGUAGE UndecidableInstances #-}

module Casimir.MonadOps
where

import Data.Kind

import Casimir.Base
import Casimir.Param

data UseBase (m :: Type -> Type)

class HasOps t where
  type family SupportedOps t :: (Type -> Type) -> Type

class
  ( HasOps t
  , Monad (OpsMonad t)
  )
  => MonadOps t where
    type family OpsMonad t :: Type -> Type

    monadOps :: SupportedOps t (OpsMonad t)

class
  ( MonadOps t
  , Monad (BaseMonad t)
  )
  => LiftMonadOps t where
    type family BaseMonad t :: Type -> Type

    liftBase :: (BaseMonad t) ~> (OpsMonad t)

class
  ( LiftMonadOps t )
  => ContraLiftMonadOps t where
    contraLiftBase
      :: ContraLift (BaseMonad t) (OpsMonad t)

withMonadOps
  :: forall t r
   . ( MonadOps t
     , MultiParam (SupportedOps t)
     )
  => (ParamConstraint (SupportedOps t) (OpsMonad t) => OpsMonad t r)
  -> OpsMonad t r
withMonadOps cont = withParam (monadOps @t) cont

instance HasOps (UseBase m) where
  type SupportedOps (UseBase m) = Nil

instance
  ( Monad m
  )
  => MonadOps (UseBase m) where
    type OpsMonad (UseBase m) = m

    monadOps = Nil

instance
  (Monad m)
  => LiftMonadOps (UseBase m) where
    type BaseMonad (UseBase m) = m

    liftBase = id
