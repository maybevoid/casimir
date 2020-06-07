
module Casimir.Freer.FreeOps
  ( FreeOps (..)
  )
where

import Casimir.Base
import Casimir.Freer.CoOp

class
  ( EffOps ops
  , EffCoOp ops
  )
  => FreeOps ops
   where
    mkFreeOps
      :: forall eff
        . (Monad eff)
      => (forall a . CoOperation ops a -> eff a)
      -> Operation ops eff
