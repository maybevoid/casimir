
module Casimir.Base.EffOps
  ( EffOps (..)
  )
where

import Data.Kind

class EffOps eff where
  type family Operation eff
    = (ops :: (Type -> Type) -> Type) | ops -> eff
