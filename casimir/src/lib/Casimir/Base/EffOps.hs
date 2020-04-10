
module Casimir.Base.EffOps
  ( EffOps (..)
  )
where

import Data.Kind

class EffOps ops where
  type family Operation ops
    = (operation :: (Type -> Type) -> Type) | operation -> ops
