{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Casimir.Higher.Sig where

import Data.Kind

type ArgKind = MonadPair

data MonadPair = MonadPair (Type -> Type) (Type -> Type)

newtype Const (f :: Type -> Type) (g :: Type -> Type) (a :: Type)  = Const
  { unConst :: f a }

type family First (x :: MonadPair) = (y :: Type -> Type) | y -> x where
  First ('MonadPair f g) = Const f g

type family Second (x :: MonadPair) = (y :: Type -> Type) | y -> x where
  Second ('MonadPair f g) = Const g f
