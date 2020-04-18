
module Casimir.Higher.Base
  ( EffOps (..)
  , HigherOps (..)
  , LowerOps (..)
  , HigherEffOps
  , LowerEffOps
  , Effect
  )
where

import Data.Kind

import Casimir.Base (Effect, EffFunctor (..))
import qualified Casimir.Base as Base

newtype LowerOps ops
  (eff :: Type -> Type)
  = LowerOps
    { unLowerOps :: ops eff eff }

newtype HigherOps ops
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)
  = HigherOps
    { unHigherOps :: ops eff2 }

class EffOps sig where
  type family Operation sig
    = (ops :: (Type -> Type) -> (Type -> Type) -> Type) | ops -> sig

class
  ( EffOps ops
  , Base.EffOps ops
  , Base.Operation ops ~ LowerOps (Operation ops)
  )
  => LowerEffOps ops

class
  ( EffOps ops
  , Base.EffOps ops
  , Operation ops ~ HigherOps (Base.Operation ops)
  )
  => HigherEffOps ops

instance
  (EffFunctor lift ops)
  => EffFunctor lift (HigherOps ops eff) where
    effmap lift (HigherOps ops) = HigherOps $
      effmap lift ops
