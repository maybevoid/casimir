
module Control.Effect.Implicit.Higher.Base
  ( EffOps (..)
  , Effect
  )
where

import Data.Kind
import Control.Effect.Implicit.Base (Effect)

class EffOps sig where
  type family Operation sig
    = (ops :: (Type -> Type) -> (Type -> Type) -> Type) | ops -> sig