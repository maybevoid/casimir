
module Casimir.Higher.Base
  ( Effect (..)
  , HigherOps (..)
  , LowerOps (..)
  , HigherEffect
  , LowerEffect
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

class Effect sig where
  type family Operation sig
    = (ops :: (Type -> Type) -> (Type -> Type) -> Type) | ops -> sig

class
  ( Effect ops
  , Base.Effect ops
  , Base.Operation ops ~ LowerOps (Operation ops)
  )
  => LowerEffect ops

class
  ( Effect ops
  , Base.Effect ops
  , Operation ops ~ HigherOps (Base.Operation ops)
  )
  => HigherEffect ops

instance
  (EffFunctor lift ops)
  => EffFunctor lift (HigherOps ops m) where
    effmap lift (HigherOps ops) = HigherOps $
      effmap lift ops
