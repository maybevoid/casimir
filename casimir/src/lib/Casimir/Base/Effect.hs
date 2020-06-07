
module Casimir.Base.Effect
  ( Effect (..)
  )
where

import Data.Kind

class Effect ops where
  type family Operation ops
    = (operation :: (Type -> Type) -> Type) | operation -> ops
