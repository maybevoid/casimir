
module Casimir.Freer.CoOp
  ( EffCoOp (..)
  )
where

import Data.Kind

class EffCoOp ops where
  type family CoOperation ops
    = (coop :: (Type -> Type)) | coop -> ops
