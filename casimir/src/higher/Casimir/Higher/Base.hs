{-# language PolyKinds #-}

module Casimir.Higher.Base
  ( Effect (..)
  , Effects (..)
  , HigherOps (..)
  , LowerOps (..)
  )
where

import Data.Kind

import Casimir.Base
  ( NoEff
  , NoOp
  , EffFunctor (..)
  , HasLabel (..)
  )

newtype LowerOps ops
  (m :: Type -> Type)
  = LowerOps
    { unLowerOps :: ops m m }

newtype HigherOps ops
  (m1 :: Type -> Type)
  (m2 :: Type -> Type)
  = HigherOps
    { unHigherOps :: ops m2 }

class Effects (eff :: k) where
  type family Operations eff
    = (ops :: (Type -> Type) -> (Type -> Type) -> Type)
    | ops -> eff

class Effect (eff :: Type) where
  type family Operation eff
    = (ops :: (Type -> Type) -> (Type -> Type) -> Type)
    | ops -> eff

instance
  (EffFunctor lift ops)
  => EffFunctor lift (HigherOps ops m) where
    effmap lift (HigherOps ops) = HigherOps $
      effmap lift ops

instance (HasLabel ops)
  => HasLabel (LowerOps ops) where
    type GetLabel (LowerOps ops) = GetLabel ops

instance Effects NoEff where
  type Operations NoEff = HigherOps NoOp
