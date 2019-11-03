
module Control.Effect.Implicit.Freer.CoOp
  ( EffCoOp (..)
  )
where

import Data.Kind
import Control.Effect.Implicit.Base

class
  (EffOps ops)
  => EffCoOp ops where
  type family CoOperation ops
    = (coop :: (Type -> Type)) | coop -> ops
