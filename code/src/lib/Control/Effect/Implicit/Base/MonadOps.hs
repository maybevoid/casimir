{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Implicit.Base.MonadOps
where

import Data.Kind

import Control.Effect.Implicit.Base.Base
import Control.Effect.Implicit.Base.Implicit
import Control.Effect.Implicit.Base.EffFunctor

class
  ( Monad (OpsMonad t) )
  => MonadOps t where
    type family HasOps t :: Type

    type family OpsMonad t :: Type -> Type

    monadOps
      :: Operation (HasOps t) (OpsMonad t)

class
  ( MonadOps t
  , ImplicitOps (HasOps t)
  , EffFunctor (Operation (HasOps t))
  ) => BaseMonadOps t

instance
  ( MonadOps t
  , ImplicitOps (HasOps t)
  , EffFunctor (Operation (HasOps t))
  ) => BaseMonadOps t

class
  ( MonadOps t
  , HigherEffFunctor (Operation (HasOps t))
  ) => HigherMonadOps t

instance
  ( MonadOps t
  , HigherEffFunctor (Operation (HasOps t))
  ) => HigherMonadOps t

withMonadOps
  :: forall t r
   . ( MonadOps t
     , ImplicitOps (HasOps t)
     )
  => (EffConstraint (HasOps t) (OpsMonad t) => OpsMonad t r)
  -> OpsMonad t r
withMonadOps cont = withOps (monadOps @t) cont
