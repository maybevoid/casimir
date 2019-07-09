
module Control.Effect.Implicit.Freer.EffCoOp
  ( EffCoOp (..)
  )
where

import Data.Kind

class EffCoOp sig where
  type family CoOperation sig
    = (coop :: (Type -> Type)) | coop -> sig
