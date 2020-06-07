
module Casimir.Base.Effect
  ( Effect (..)
  , Effects (..)
  , Singleton
  )
where

import Data.Kind

import QuasiParam.Casimir as Param

data Singleton eff

class Effects eff where
  type family Operations eff
    = (ops :: (Type -> Type) -> Type) | ops -> eff

class Effect eff where
  type family Operation eff
    = (ops :: (Type -> Type) -> Type) | ops -> eff

instance
  ( Effect eff )
  => Effects (Singleton eff) where
    type Operations (Singleton eff) = Param.Cell (Operation eff)
