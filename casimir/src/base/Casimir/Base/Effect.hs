
module Casimir.Base.Effect
  ( Effects (..)
  )
where

import Data.Kind

class Effects ops where
  type family Operations ops
    = (operation :: (Type -> Type) -> Type) | operation -> ops
