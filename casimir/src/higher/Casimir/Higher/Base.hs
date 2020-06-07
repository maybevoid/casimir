
module Casimir.Higher.Base
  ( Effects (..)
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

class Effects sig where
  type family Operations sig
    = (ops :: (Type -> Type) -> (Type -> Type) -> Type) | ops -> sig

class
  ( Effects ops
  , Base.Effects ops
  , Base.Operations ops ~ LowerOps (Operations ops)
  )
  => LowerEffect ops

class
  ( Effects ops
  , Base.Effects ops
  , Operations ops ~ HigherOps (Base.Operations ops)
  )
  => HigherEffect ops

instance
  (EffFunctor lift ops)
  => EffFunctor lift (HigherOps ops m) where
    effmap lift (HigherOps ops) = HigherOps $
      effmap lift ops
