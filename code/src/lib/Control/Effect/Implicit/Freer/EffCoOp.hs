
module Control.Effect.Implicit.Freer.EffCoOp
  ( EffCoOp (..)
  )
where

import Data.Kind

class EffCoOp (ops :: (Type -> Type) -> Type) where
  type family CoOperation ops
    = (coop :: (Type -> Type)) | coop -> ops
