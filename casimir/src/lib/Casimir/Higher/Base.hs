
module Casimir.Higher.Base
  ( EffOps (..)
  , HigherOps (..)
  , LowerOps (..)
  , HigherEffOps
  , LowerEffOps
  )
where

import Data.Kind

import Casimir.Base (EffFunctor (..))
import qualified Casimir.Base as Base

newtype LowerOps ops
  (m :: Type -> Type)
  = LowerOps
    { unLowerOps :: ops m m }

newtype HigherOps ops
  (m1 :: Type -> Type)
  (m2 :: Type -> Type)
  = HigherOps
    { unHigherOps :: ops m2 }

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
  => EffFunctor lift (HigherOps ops m) where
    effmap lift (HigherOps ops) = HigherOps $
      effmap lift ops
