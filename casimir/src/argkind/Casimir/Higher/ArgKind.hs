{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Casimir.Higher.ArgKind where

import Data.Kind (Type)

data MonadPair = MonadPair (Type -> Type) (Type -> Type)

type ArgKind = MonadPair

type Operation' ops m = ops ('MonadPair m m)
type Operation ops m1 m2 = ops ('MonadPair m1 m2)

type family M1 (p :: MonadPair) = (m :: Type -> Type) where
  M1 ('MonadPair m _) = m

type family M2 (p :: MonadPair) = (m :: Type -> Type) where
  M2 ('MonadPair _ m) = m

class MonadConst
  (p :: MonadPair)
  (m :: Type -> Type)
  | p -> m, m -> p

instance MonadConst ('MonadPair m m) m where
