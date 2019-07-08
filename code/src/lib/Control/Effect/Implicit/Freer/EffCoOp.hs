
module Control.Effect.Implicit.Freer.EffCoOp
  ( FreerEffCoOp (..)
  )
where

import Data.Kind

class FreerEffCoOp sig where
  type family FreerCoOp sig
    = (coop :: (Type -> Type)) | coop -> sig
