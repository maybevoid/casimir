
module Control.Effect.Implicit.Base.Base
  ( EffOps (..)
  )
where

import Data.Kind

class EffOps sig where
  type family Operation sig
    = (ops :: (Type -> Type) -> Type) | ops -> sig